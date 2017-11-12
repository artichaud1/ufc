library(foreach)
library(dplyr)

source('scrape_events.R')
source('scrape_fight.R')

scrape_fights <- function(){
  
  events_df <- scrape_events()
  
  foreach(url = events_df[1:2,]$url) %do% tryCatch(scrape_fight(url), error=function(url) NULL) %>%
    bind_rows
  
}
