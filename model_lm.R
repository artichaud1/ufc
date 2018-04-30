library(stringr)
library(dplyr)
library(lubridate)
library(recipes)
library(rsample)
library(purrr)
library(magrittr)

source('featurize.R')

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
    gender = ifelse(str_detect(Weight.class, 'Women'), 'female', 'male'),
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
  step_novel(all_nominal()) %>%
  step_dummy(all_nominal(), one_hot = FALSE) %>%
  step_scale(all_predictors(), -all_nominal()) %>%
  step_zv(all_predictors())

train_samples_df <- 
  train_df %>%
  rolling_origin(initial = 3000, assess = 500, cumulative = FALSE, skip = 100)

train_samples_df$recipes <- map(train_samples_df$splits, 
                                prepper, 
                                recipe = rec,
                                retain = TRUE,
                                verbose = FALSE)

train_samples_df$lm_mod <- 
  map(
    train_samples_df$recipes,
    function(rec) glm(target ~ ., family = binomial(link='logit'), data = juice(rec))
  )

train_samples_df$pred <- 
  pmap(
    lst(
      split_obj = train_samples_df$splits,
      rec_obj = train_samples_df$recipes,
      mod_obj = train_samples_df$lm_mod
    ),
    function(split_obj, rec_obj, mod_obj){
      eval_df <- bake(rec_obj, newdata = assessment(split_obj))
      preds <- predict(mod_obj, newdata = eval_df, type = 'response')
      bind_cols(target = eval_df$target, predicted = preds)
    }
  )

train_samples_df$accuracy <- 
  map_dbl(
    train_samples_df$pred,
    function(df) mean(ifelse(df$predicted > 0.5, 1, 0) == df$target)
  )
