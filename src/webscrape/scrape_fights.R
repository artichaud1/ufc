library(foreach)
library(dplyr)
library(rowr)
library(readr)
library(rvest)
library(magrittr)
library(anytime)

source('src/webscrape/scrape_event_list.R')
source('src/webscrape/scrape_event.R')
source('src/webscrape/scrape_fight.R')

source('~/R/projects/listr/do_batch.R')


scrape_fights <- function(n_events=Inf, 
                          verbose = TRUE){
  
  # Read in the list of events
  events_df <- scrape_event_list()
  
  # Remove future events
  events_df %<>%
    filter(
      anydate(Date) <= lubridate::today()
    )
  
  # If we have already scraped some events, skip them
  alrdy_scraped_events_df <- read_batch('data/webscraped')
  
  if(!is.null(alrdy_scraped_events_df) && nrow(alrdy_scraped_events_df) > 0){
    events_df %<>%
      anti_join(alrdy_scraped_events_df, by = c('Url' = 'Event_url'))
  }
  
  n_events <- min(n_events, nrow(events_df))
  
  # Take only n_events if it was specified
  events_df <- events_df[1:n_events, ]
  
  if(verbose){
    if(n_events > 0){
      print(paste("Scraping", n_events, "out of", nrow(events_df), "new events."))
    }else{
      print("No events to scrape. Exiting.")
    }
  } 
  
  try_scrape_fight <- function(url){
    tryCatch(scrape_fight(url),
             error=function(url) NULL)
  }
  
  # Scrape all fights related to an event
  try_scrape_event <- function(url, date, event_number){
    
    if(verbose) print(paste("Event", event_number, "/", n_events))
    
    event_df <- 
      tryCatch(
        scrape_event(url) %>% mutate(Date = date),
        error=function(url) NULL
      )
    
    if(is.null(event_df)) return(NULL)
    
    n_fights <- nrow(event_df)
    
    fights_df <- 
      foreach(url = event_df$Fight_url,
              i = 1:n_fights) %do% {
                
        if(verbose) print(paste("Fight", i, "/", n_fights))
  
        tryCatch(scrape_fight(url),
                 error=function(url) NULL)
  
        } %>%
      bind_rows
    
    cbind.fill(event_df, fights_df, fill=NA)
  }

  new_batch <- 
    do_batch(
      try_scrape_event,
      args = list(
        url = events_df$Url[1:n_events],
        date = events_df$Date[1:n_events],
        event_number = 1:n_events
      ),
      outfolder = 'data/webscraped/',
      batchsize = 50, 
      verbose = verbose
    )
  
  bind_rows(alrdy_scraped_events_df %>% mutate_if(is.factor, as.character), 
            new_batch %>% mutate_if(is.factor, as.character))
}

# fights <- scrape_fights()
# fights %>% write_csv('fights.csv')

