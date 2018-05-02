library(tidyr)
library(magrittr)
library(stringr)
library(anytime)
library(readr)
library(dplyr)
library(recipes)
library(rlang)

source('scrape_fights.R')


# Preprocessing functions -------------------------------------------------

`%nin%` <- purrr::negate(`%in%`)

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

create_fights_df <- function(fights_scraped){
  
  vars_xofy <- c('SigStrikes', 'TotStrikes', 'Td', 'Head', 'Body', 
                 'Leg', 'Distance', 'Clinch', 'Ground')
  vars_xofy <- c(paste0(vars_xofy,   '1'), paste0(vars_xofy, '2'))
  vars_to_num <- c('SubAtt', 'Kd', 'Rev', 'Pass')
  vars_to_num <- c(paste0(vars_to_num, '1'), paste0(vars_to_num, '2'))
  
  fights_df <- 
    fights_scraped %>%
    split_xofy(vars_xofy) %>%
    to_numeric(vars_to_num) %>%
    make_date %>%
    make_mins %>%
    mutate(
      Method = case_when(
        str_detect(Method, "^SUB") ~ 'SUB',
        str_detect(Method, "^KO") ~ 'KO',
        str_detect(Method, "^Overturned") ~ 'Overturned',
        str_detect(Method, "DQ") ~ 'DQ',
        TRUE ~ as.character(Method) %>% str_trim
      ) %>% factor
    ) %>%
    mutate(
      Wins1 = case_when(
        str_trim(W.L) == "win" ~ 1,
        TRUE ~ 1
      ),
      Wins2 = case_when(
        str_trim(W.L) == "win" ~ 0,
        TRUE ~ 1
      ) 
    ) %>%
    select(-W.L)
  
  # Dummify winning Method for fighter 1
  fights_df <- 
    recipe(fights_df) %>%
    step_other(Method, threshold = 0.05) %>%
    step_dummy(Method, one_hot = TRUE) %>%
    prep(retain = TRUE) %>%
    juice %>%
    rename_at(.vars = vars(starts_with('Method')), .funs = function(x) paste0(x, '1'))
  
  # Set winning method dummies to 0 for fighter 2
  cnames <- colnames(fights_df)
  dummies <- cnames[startsWith(cnames, 'Method')]
  newdummies <- paste0(str_sub(dummies, 1, -2), 2)
  for(newdummy in newdummies) fights_df[[newdummy]] <- 0
  
  fights_df
}

  
# Add 'against' variables -------------------------------------------------
  
add_against_vars <- function(fights_df){
  
  vars_fighter1 <- colnames(fights_df)[endsWith(colnames(fights_df), '1')]
  vars_fighter2 <- colnames(fights_df)[endsWith(colnames(fights_df), '2')]
  
  vars_against_fighter1 <- setdiff(vars_fighter1, 'Fighter1')
  for(var in vars_against_fighter1){
    new_var <- str_sub(var, 1, -2)
    fights_df[[paste0(new_var, '_Against', 2)]] <- fights_df[[var]]
  }
  
  vars_against_fighter2 <- setdiff(vars_fighter2, 'Fighter2')
  for(var in vars_against_fighter2){
    new_var <- str_sub(var, 1, -2)
    fights_df[[paste0(new_var, '_Against', 1)]] <- fights_df[[var]]
  }
  
  fights_df
}



# Split fights dataframe into fighter1 and fighter2 dataframes and normalize column names ------------

split_fighters <- function(fights_df){
  
  vars_fighter1 <- colnames(fights_df)[endsWith(colnames(fights_df), '1')]
  vars_fighter2 <- colnames(fights_df)[endsWith(colnames(fights_df), '2')]
  
  common_vars <- c('Weight.class', 'Round', 'Mins', 'Fight_url', 'Date')
  fighter1_vars <- c(common_vars, vars_fighter1)
  fighter2_vars <- c(common_vars, vars_fighter2)
  
  fighter1_df <- 
    fights_df %>%
    select(one_of(fighter1_vars)) %>%
    mutate(FighterFlag = 1) %>%
    mutate_if(is.factor, as.character)
  
  colnames(fighter1_df) <- ifelse(endsWith(colnames(fighter1_df), '1'), 
                                  str_sub(colnames(fighter1_df), end = -2),
                                  colnames(fighter1_df))
  
  fighter2_df <- 
    fights_df %>%
    select(one_of(fighter2_vars)) %>%
    mutate(FighterFlag = 2) %>%
    mutate_if(is.factor, as.character)
  
  colnames(fighter2_df) <- ifelse(endsWith(colnames(fighter2_df), '2'), 
                                  str_sub(colnames(fighter2_df), end = -2),
                                  colnames(fighter2_df))
  
  list(fighter1_df, fighter2_df)
  
}


# Regroup the two dataframes into one and compute cumulative stats --------

regroup_cumul <- function(fighter_12){
  
  fighters_df <- 
    bind_rows(fighter_12) %>%
    group_by(Fighter) %>%
    arrange(Date) %>%
    mutate(N_Fights = 1)
  
  for(col in colnames(fighters_df) %>% setdiff('FighterFlag')){
    if(is.numeric(fighters_df[[col]])){
      fighters_df$old_col <- fighters_df[[col]]
      fighters_df %<>% 
        mutate(new_col = cumsum(old_col))
      fighters_df[[paste('Cume', col, sep = '_')]] <- fighters_df$new_col
    }
  }
  
  fighters_df %>% 
    select(-old_col, -new_col) %>%
    arrange(Fighter, Date) %>%
    na.omit
  
}

# Compute cumulative win ratios, including by method ---------------------------------

compute_win_ratios <- function(fighters_df){
  
  cols <- colnames(fighters_df)
  numerators <- cols[str_detect(cols, 'Cume')] %>% 
    intersect(cols[str_detect(cols, "Wins|Method")])
  
  for(col in syms(numerators)){
    newcol <- paste(col, 'Ratio', sep = '_')
    
    fighters_df %<>%
      mutate(
        !!newcol := (!!col)/Cume_N_Fights
      )
  }
  
  fighters_df
}


# Compute lagged cumulative stats ------------------------------------------------

compute_lagged_stats <- function(fighters_df){
  
  cols <- colnames(fighters_df)
  stats_cols <- cols[startsWith(cols, 'Cume')] 
  
  fighters_df %<>% 
    group_by(Fighter) %>%
    arrange(Fighter, Date)
  
  for(col in syms(stats_cols)){
    newcol <- paste0('Prev_', col)
    fighters_df %<>% mutate(!!newcol := lag(!!col))
  }
  
  fighters_df
}




# For modeling purposes, split back into fighter1 and fighter2 dataframes ---------------
# Keeping the Prev_Cume_ variables

resplit_fighters12 <- funtion(fighters_df){
  
  fighter1_df <- 
    fighters_df %>%
    ungroup %>%
    filter(FighterFlag == 1) %>%
    select(c(one_of('Fight_url', 'Date'), starts_with('Prev_Cume')))
  
  colnames(fighter1_df)[startsWith(colnames(fighter1_df), 'Prev_Cume')] <- 
    paste0(colnames(fighter1_df)[startsWith(colnames(fighter1_df), 'Prev_Cume')], '_1')
  
  fighter2_df <- 
    fighters_df %>%
    ungroup %>%
    filter(FighterFlag == 2) %>%
    select(c(one_of('Fight_url', 'Date'), starts_with('Prev_Cume')))
  
  colnames(fighter2_df)[startsWith(colnames(fighter2_df), 'Prev_Cume')] <- 
    paste0(colnames(fighter2_df)[startsWith(colnames(fighter2_df), 'Prev_Cume')], '_2')
  
  list(fighter1_df, fighter2_df)
}



# Merge cumulative stats onto initial fights dataframe --------------------

merge_back_stats <- function(fights_df, fighter12){
  
  fights_df %>%
    left_join(fighter12[[1]], by = c('Fight_url', 'Date')) %>%
    left_join(fighter12[[2]], by = c('Fight_url', 'Date'))
}


# Create modeling dataset -------------------------------------------------

create_model_dataset <- function(fights_df){
  
  fights_1_df <- fights_df %>% select(ends_with('1'))
  fights_2_df <- fights_df %>% select(ends_with('2'))
  fights_other_df <- fights_df[, colnames(fights_df) %nin% 
                                 c(colnames(fights_1_df), colnames(fights_2_df))]
  
  fights_1_df %<>% rename_all(~ str_replace(., '1', '2'))
  fights_2_df %<>% rename_all(~ str_replace(., '2', '1'))
  
  fights_reverse_df <- 
    bind_cols(fights_other_df, fights_1_df, fights_2_df) 
  
  bind_rows(fights_df %>% mutate(target = 1) %>% mutate_if(is.factor, as.character),
            fights_reverse_df %>% mutate(target = 0) %>% mutate_if(is.factor, as.character))
  
}


# Call functions ----------------------------------------------------------

# fights_scraped <- scrape_fights() 
# fights_scraped %>% saveRDS('fights_scraped_df.RDS')
# fights_scraped %>% write.csv('fights_scraped.csv', row.names = FALSE)


# Compute stats & win ratios, including lagged values.
fighters_df <- 
  readRDS('fights_scraped_df.RDS') %>%
  na.omit() %>%
  create_fights_df() %>%
  add_against_vars() %>% 
  split_fighters() %>% 
  regroup_cumul() %>%
  compute_win_ratios() %>%
  compute_lagged_stats() %>%
  write_rds('fighters_df.RDS') %>%
  write_csv('fighters.csv') 


# Save a version with only most recent stats for each fighter
fighters_df %>%
  slice(n()) %>%
  write_rds('fighters_cumul_df.RDS') %>%
  write_csv('fighters_cumul.csv')

# Create modeling dataset
fighters_df %>% 
  resplit_fighters12() %>%
  merge_back_stats() %>%
  save_rds('fights_df.RDS') %>%
  write_csv('fights.csv') %>%
  create_model_dataset() %>%
  write_rds('fights_model_df.RDS') %>%
  write_csv('fights_model.csv')


# Save datasets to dropbox ------------------------------------------------

library(rdrop2)
token <- drop_auth()
saveRDS(token, file = "~/dropbox_token.rds")

# create ufc folder if doesn't exist
res <- drop_dir(dtoken = token) %>% filter(.tag == "folder" & name == 'ufc')
if(nrow(res) == 0) drop_create('ufc', dtoken = token)
drop_upload('fights_model.csv', path = 'ufc', dtoken = token)
drop_upload('fights.csv', path = 'ufc', dtoken = token)
drop_upload('fighters.csv', path = 'ufc', dtoken = token)
drop_upload('fighters_cumul.csv', path = 'ufc', dtoken = token)
