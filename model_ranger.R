library(stringr)
library(dplyr)
library(lubridate)
library(recipes)
library(rsample)
library(purrr)
library(magrittr)
library(ranger)
library(ggplot2)
library(ggthemes)
library(foreach)


source('featurize.R')
source('tidy_grid.R')
source('metrics.R')

raw_df <- readRDS('fights_model_df.RDS')

# Exclude 1st fights from modeling data
except_1st_fights_df <- raw_df %>% filter(!is.na(Prev_Cume_Mins_1), !is.na(Prev_Cume_Mins_2))

# Now any remaining NA's with zeros
no_NAs_df <- except_1st_fights_df %>% replace(is.na(.), 0)

full_df <- 
  no_NAs_df %>%
  select(
    target,
    Weight.class,
    Date,
    intersect(starts_with('Prev_Cume'), contains('_PM')),
    intersect(starts_with('Prev_Cume'), contains('_Ratio'))
  ) %>%
  replace(is.na(.), 0) %>%
  mutate(
    target = factor(target),
    gender = factor(ifelse(str_detect(Weight.class, 'Women'), 'female', 'male')),
    Weight.class = factor(str_trim(str_replace(Weight.class, "Women's", '')))
  ) 

full_features_df <- 
  full_df %>%
  featurize_fight_advantages() %>%
  select(target,
         Date,
         starts_with('Adv'), 
         gender,
         starts_with('Weight.class'))

train_df <- full_features_df %>% filter(year(Date) < 2017) %>% arrange(Date) %>% select(-Date)
test_df <- full_features_df %>% filter(year(Date) >= 2017) %>% arrange(Date) %>% select(-Date)

rec <- 
  recipe(train_df) %>%
  add_role(target, new_role = 'outcome') %>%
  add_role(-target, new_role = 'predictor') %>%
  step_novel(all_nominal(), -target) %>%
  step_dummy(all_nominal(), -target, one_hot = TRUE) %>%
  step_zv(all_predictors())

train_samples_df <- 
  train_df %>%
  rolling_origin(initial = 3000, assess = 500, cumulative = FALSE, skip = 500)

train_samples_df$recipes <- map(train_samples_df$splits, 
                                prepper, 
                                recipe = rec,
                                retain = TRUE,
                                verbose = FALSE)


train_ranger <- function(train_df, params){
  ranger(data = train_df, 
         dependent.variable.name = target,
         num.trees = 50,
         write.forest = TRUE,
         probability = TRUE,
         min.node.size = params$min.node.size,
         mtry = params$mtry)
}

predict_ranger <- function(model, eval_df){
  predict(model, eval_df, type = 'response')$predictions[, '1']
}

param_grid <- cross(list(mtry = c(10,5), min.node.size=c(10,50)))

accuracy <- function(predicted, target, threshold = 0.5){
  mean(ifelse(predicted > threshold, 1, 0) == target)
} 

tuning_results <- 
  grid_search(train_samples_df, 
              'target', 
              param_grid, 
              train_ranger,
              predict_ranger,
              list(acc = accuracy),
              threshold = 0.70)
  






# tuning_results <- 
#   train_samples_df %>%
#   group_by(
#     mtry = map_dbl(params, 'mtry'),
#     min.node.size = map_dbl(params, 'min.node.size')
#   ) %>%
#   summarize(
#     accuracy = mean(accuracy)
#   )
# 
# ggplot(data = tuning_results) + 
#   geom_point(aes(x = min.node.size, y = accuracy, col = factor(mtry), group = factor(mtry))) + 
#   geom_line(aes(x = min.node.size, y = accuracy, col = factor(mtry), group = factor(mtry))) + 
#   theme_bw()
# 
#   