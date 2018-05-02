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


raw_df <- readRDS('fights_model_df.RDS')

# Exclude 1st fights from modeling data
except_1st_fights_df <- raw_df %>% filter(!is.na(Prev_Cume_Mins_1), !is.na(Prev_Cume_Mins_2))

# Now replace any remaining NA's with zeros
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


train_ranger <- function(train_df, target, params){
  ranger(data = train_df, 
         dependent.variable.name = target,
         num.trees = params$num.trees,
         write.forest = TRUE,
         probability = TRUE,
         min.node.size = params$min.node.size,
         mtry = params$mtry)
}

predict_ranger <- function(model, eval_df){
  predict(model, eval_df, type = 'response')$predictions[, '1']
}

param_grid <- make_params(mtry = 3:7, 
                          min.node.size = c(50, 100, 200),
                          num.trees = c(100, 200))

tuning_results <- 
  grid_search(train_samples_df, 
              'target', 
              param_grid, 
              train_ranger,
              predict_ranger,
              list(acc = accuracy),
              threshold = 0.50) 

best_paramset <- 
  tuning_results %>%
  group_by(paramset) %>%
  summarise(acc = mean(acc)) %>%
  filter(acc == max(acc)) %>%
  pull(paramset)

best_tuning_results <- 
  tuning_results %>%
  filter(paramset == best_paramset)

roc_results <- 
  best_tuning_results %>%
  mutate(
    roc = map(
      pred,
      ~ roc(.$predicted, .$target)
    )
  ) %>%
  unnest(roc) %>%
  group_by(model, threshold) %>%
  summarise(
    precision = mean(precision),
    recall = mean(recall),
    tpr = mean(tpr),
    fpr = mean(fpr)
  )

plot_roc(roc_results, 'fpr', 'tpr')

# Code below is relevant to visualize tuning results
#
# tuning_results_summ <- 
#   tuning_results %>%
#   group_by_at(vars(-id, -pred, -acc)) %>%
#   summarise(acc = mean(acc))
#   
# ggplot(data = tuning_results_summ,
#        aes(x = num.trees, y = acc, col = factor(min.node.size))) + 
#   geom_point() + 
#   geom_line() +
#   theme_light()


# Code below is relevant to train final model on train_df and test on test_df

# prepped_rec <- prep(rec, retain = TRUE)
# baked_train_df <- as.data.frame(juice(prepped_rec))
# baked_test_df <- bake(prepped_rec, test_df)
# 
# model <- train_ranger(baked_train_df, 
#                       target = 'target', 
#                       params = list(
#                         mtry = 6, 
#                         min.node.size = 200,
#                         num.trees = 100
#                       ))
# 
# preds <- predict_ranger(model, baked_test_df)
# 
# res_df <- 
#   bind_cols(target = as.numeric(as.character(baked_test_df[['target']])), predicted = preds)
# 
# accuracy(res_df$predicted, res_df$target)
# roc_data <- roc(res_df$predicted, res_df$target, model = 'ranger') 
# 
# roc_data %>% plot_roc('recall', 'precision')
