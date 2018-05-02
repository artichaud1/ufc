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


source('./featurize.R')


make_features <- function(raw_df){
  
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
  
  full_df %>%
    featurize_fight_advantages() %>%
    select(target,
           Date,
           starts_with('Adv'), 
           gender,
           starts_with('Weight.class')) %>%
    arrange(Date) %>%
    select(-Date)
}


make_recipe <- function(features_df){
  recipe(features_df) %>%
    add_role(target, new_role = 'outcome') %>%
    add_role(-target, new_role = 'predictor') %>%
    step_novel(all_nominal(), -target) %>%
    step_dummy(all_nominal(), -target, one_hot = TRUE) %>%
    step_zv(all_predictors())
}


make_resamples <- function(features_df, recipe){
  resamples_df <- 
    features_df %>%
    rolling_origin(initial = 4000, assess = 400, cumulative = FALSE, skip = 500)
  
  resamples_df$recipes <- map(resamples_df$splits, 
                              prepper, 
                              recipe = rec,
                              retain = TRUE,
                              verbose = FALSE)
}


train_predict_ranger <- function(train_df, target, params, eval_df){
  model <- 
    ranger(data = train_df, 
         dependent.variable.name = target,
         num.trees = params$num.trees,
         write.forest = TRUE,
         probability = TRUE,
         min.node.size = params$min.node.size,
         mtry = params$mtry)
  
  predict(model, eval_df, type = 'response')$predictions[, '1']
}


