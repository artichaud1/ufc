library(foreach)
library(dplyr)
library(rowr)

source('scrape_events.R')
source('scrape_fight.R')
source('scrape_fight_details.R')

# Note: n_events should be at least 2, because there is a `Next` event
scrape_fights <- function(n_events=Inf, n_fights=Inf){
  
  events_df <- scrape_events()
  
  n_events <- pmin(n_events, nrow(events_df))
  
  fights_df <- 
    foreach(url = events_df$url[1:n_events]) %do% tryCatch(scrape_fight(url), 
                                                            error=function(url) NULL) %>%
    bind_rows
  
  n_fights <- pmin(n_fights, nrow(fights_df))
  
  fights_details_df <- 
    foreach(url = fights_df$Fight_url[1:n_fights]) %do% tryCatch(scrape_fight_details(url), 
                                                                 error=function(url) NULL) %>%
    bind_rows
  
  cbind.fill(fights_df, fights_details_df, fill=NA)
}

# View(scrape_fights(n_events = 2, n_fights = 2))
