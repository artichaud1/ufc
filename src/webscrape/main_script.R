#### Params ----
save_outputs = FALSE
save_dropbox = FALSE


library(readr)
library(magrittr)

source('src/webscrape/main_functions.R')

source('src/webscrape/scrape_fights.R')
fights_scraped <- scrape_fights()
fights_scraped %>% saveRDS('data/fights_scraped_df.RDS')
fights_scraped %>% write.csv('data/fights_scraped.csv', row.names = FALSE)


# Compute stats & win ratios, including lagged values.
fights_df <- 
  readRDS('data/fights_scraped_df.RDS') %>%
  na.omit() %>%
  create_fights_df() %>%
  {if(save_outputs){
    write_rds(., 'data/fights_df.RDS') %>%
    write_csv(., 'data/fights.csv')}
  }


fighters_df <- 
  fights_df %>%
  add_against_vars() %>% 
  split_fighters() %>% 
  regroup_cumul() %>%
  compute_win_ratios() %>%
  per_min_stats() %>%
  compute_lagged_stats() %>%
  {if(save_outputs){
    write_rds(., 'data/fighters_df.RDS') %>%
    write_csv(., 'data/fighters.csv')}
  }


# Save a version with only most recent stats for each fighter
fighters_cumul_df <- 
  fighters_df %>%
  slice(n()) %>%
  {if(save_outputs){
    write_rds(., 'fighters_cumul_df.RDS') %>%
    write_csv(., 'fighters_cumul.csv')}
  }

# Create modeling dataset
fights_model_df <- 
  fighters_df %>% 
  resplit_fighters12() %>%
  merge_back_stats(fights_df) %>%
  reverse_bind() %>%
  {if(save_outputs){
    write_rds(., 'data/fights_model_df.RDS') %>%
    write_csv(., 'data/fights_model.csv')}
  }


# Save datasets to dropbox ------------------------------------------------

if(save_dropbox){
  
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
}
