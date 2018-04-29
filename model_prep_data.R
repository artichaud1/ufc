library(stringr)
library(dplyr)
library(lubridate)
library(recipes)

data_raw <- readRDS('fights_model_df.RDS')

full_df <- 
  data_raw %>%
  select(
    target,
    Weight.class,
    Date,
    intersect(starts_with('Prev_Cume'), ends_with('PM')),
    intersect(starts_with('Prev_Cume'), ends_with('Ratio'))
  ) %>%
  replace(is.na(.), 0) %>%
  mutate(
    gender = ifelse(str_detect(Weight.class, 'Women'), 0, 1)#,
    # Weight.class = as.numeric(
    #                   factor(str_trim(str_replace(Weight.class, "Women's", '')),
    #                          levels = c('Strawweight', 'Flyweight', 'Bantamweight',
    #                                     'Featherweight', 'Lightweight', 'Welterweight',
    #                                     'Middleweight', 'Light Heavyweight', 'Heavyweight',
    #                                     'Super Heavyweight', 'Catch Weight', 'Open Weight')))
  ) 

train_df <- full_df %>% filter(year(Date) < 2017) 
test_df <- full_df %>% filter(year(Date) >= 2017) 

rec <- 
  recipe(full_df) %>%
  add_role(target, new_role = 'outcome') %>%
  add_role(-target, -Date, new_role = 'predictor') %>%
  add_role(Date, new_role = 'date') %>%
  step_dummy(Weight.class, one_hot = TRUE) %>%
  step_scale(all_predictors()) %>%
  prep(training = train_df, retain = TRUE)

