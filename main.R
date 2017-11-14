library(tidyr)
library(magrittr)
library(stringr)
library(anytime)
library(readr)
library(dplyr)

source('scrape_fights.R')


# Preprocessing functions -------------------------------------------------

split_xofy <- function(fights, cols){
  
  for(col in cols){
    fights %<>%
      separate(col, sep = "of", into = c(col, paste('Total', col, sep='_')), convert = TRUE) 
    
    #fights[[paste('Perc', col, sep='_')]] <- fights[[col]] / fights[[paste('Total', col, sep='_')]]
  }
  
  fights
}


make_diffs <- function(fights){
  
  cols1 <- colnames(fights)[endsWith(colnames(fights), '1')] %>% setdiff('Fighter1')
  cols2 <- colnames(fights)[endsWith(colnames(fights), '2')] %>% setdiff('Fighter2')
  
  new_cols <- paste('Diff', cols1, sep = '_') %>% str_sub(end = -2)
  
  for(i in 1:length(new_cols)){
    fights[[new_cols[i]]] <- fights[[cols2[i]]] - fights[[cols1[i]]]
  }
  
  fights
}


to_numeric <- function(fights, cols){
  
  for(col in cols){
    fights[[col]] <- as.numeric(as.character(fights[[col]]))
  }
  
  fights
}


make_mins <- function(fights){
  fights %>%
    separate('Time', into = c('Mins', 'Secs'), convert = TRUE) %>%
    mutate(Mins = Mins + Secs/60) %>%
    select(-Secs)
}


per_min_stats <- function(fights){
  
  
  
  num_vars <- colnames(fights)[map_lgl(fights, is.numeric)] %>% setdiff(c('Round', 'Mins'))
  
  for(num_var in num_vars) fights[[num_var]] <- fights[[num_var]] / fights$Mins
  
  fights
}


make_date <- function(fights){
  fights %>% mutate(Date = anydate(Date))
}


# Main script -------------------------------------------------------------


fights_scraped <- scrape_fights()
fights_scraped %>% saveRDS('fights_scraped.csv')

vars_xofy <- c('SigStrikes', 'TotStrikes', 'Td', 'Head', 'Body', 'Leg', 'Distance', 'Clinch', 'Ground')
vars_xofy <- c(paste0(vars_xofy,   '1'), paste0(vars_xofy, '2'))
vars_to_num <- c('SubAtt', 'Kd', 'Rev', 'Pass')
vars_to_num <- c(paste0(vars_to_num, '1'), paste0(vars_to_num, '2'))

fights_df <- 
  fights_scraped %>%
  split_xofy(vars_xofy) %>%
  to_numeric(vars_to_num) %>%
  make_date %>%
  make_mins %>%
  mutate_if(is.factor, as.character)

# Split fights dataframe into fighter1 and fighter2 dataframes ------------

common_vars <- c('W.L', 'Weight.class', 'Method', 'Round', 'Mins', 'Fight_url', 'Date')
fighter1_vars <- c(common_vars, colnames(fights_df)[endsWith(colnames(fights_df), '1')])
fighter2_vars <- c(common_vars, colnames(fights_df)[endsWith(colnames(fights_df), '2')])

fighter1_df <- 
  fights_df %>%
  select(one_of(fighter1_vars)) %>%
  mutate(FighterFlag = 1)

colnames(fighter1_df) <- ifelse(endsWith(colnames(fighter1_df), '1'), 
                                str_sub(colnames(fighter1_df), end = -2),
                                colnames(fighter1_df))

fighter2_df <- 
  fights_df %>%
  select(one_of(fighter2_vars)) %>%
  mutate(FighterFlag = 2) 

colnames(fighter2_df) <- ifelse(endsWith(colnames(fighter2_df), '2'), 
                                str_sub(colnames(fighter2_df), end = -2),
                                colnames(fighter2_df))

# Regroup the two dataframes into one and compute cumulative stats --------

fighters_df <- 
  bind_rows(fighter1_df, fighter2_df) %>%
  group_by(Fighter) %>%
  arrange(Date) 

for(col in colnames(fighters_df) %>% setdiff('FighterFlag')){
  if(is.numeric(fighters_df[[col]])){
    fighters_df$old_col <- fighters_df[[col]]
    fighters_df %<>% mutate(new_col = cumsum(old_col))
    fighters_df[[paste('Cume', col, sep = '_')]] <- fighters_df$new_col
  }
}

fighters_df %<>% 
  select(-old_col, -new_col) %>%
  arrange(Fighter, Date)


# Compute per minute stats ------------------------------------------------
cols <- colnames(fighters_df)
stats_cols <- 
  cols[startsWith(cols, 'Cume')] %>%
  setdiff(c('Cume_Round', 'Cume_Mins'))

for(col in stats_cols){
  fighters_df[[paste0(col, '_PM')]] <- fighters_df[[col]] / fighters_df[['Cume_Mins']]
}


# Split back into fighter1 and fighter2 dataframes ------------------------

fighter1_df <- 
  fighters_df %>%
  ungroup %>%
  filter(FighterFlag == 1) %>%
  select(c(one_of('Fight_url', 'Date'), starts_with('Cume')))

colnames(fighter1_df)[startsWith(colnames(fighter1_df), 'Cume')] <- 
  paste0(colnames(fighter1_df)[startsWith(colnames(fighter1_df), 'Cume')], '1')

fighter2_df <- 
  fighters_df %>%
  ungroup %>%
  filter(FighterFlag == 2) %>%
  select(c(one_of('Fight_url', 'Date'), starts_with('Cume')))

colnames(fighter2_df)[startsWith(colnames(fighter2_df), 'Cume')] <- 
  paste0(colnames(fighter2_df)[startsWith(colnames(fighter2_df), 'Cume')], '2')


# Merge cumulative stats onto initial fights dataframe --------------------

fights_df %<>%
  left_join(fighter1_df, by = c('Fight_url', 'Date')) %>%
  left_join(fighter2_df, by = c('Fight_url', 'Date'))

fights_df %>% saveRDS('fights.RDS')
#fights_df <- readRDS('fights.csv')


# Compute Diff per minute stats for the winner (fighter1) -----------------

cols1 <- 
  colnames(fights_df) %>%
  extract(startsWith(., 'Cume') & endsWith(., 'PM1'))

cols2 <- 
  colnames(fights_df) %>%
  extract(startsWith(., 'Cume') & endsWith(., 'PM2'))

for(k in 1:length(cols1)){
  fights_df[[paste0('Diff_', str_sub(cols1[k], end = -2))]] <- 
    fights_df[[cols1[k]]] - fights_df[[cols2[k]]]
}

fights1_df <- 
  fights_df %>%
  select(c(one_of('Weight.class'), starts_with('Diff'))) %>%
  mutate(target = 1)

# Compute Diff per minute stats for the loser fighter2 ---------------------

fights2_df <- 
  fights1_df %>%
  mutate_if(is.numeric, ~ -.x) %>%
  mutate(target = 0)


# Create modeling dataset -------------------------------------------------

fights_model_df <- bind_rows(fights1_df, fights2_df)
fights_model_df %>% saveRDS('fights_model_df.RDS')
