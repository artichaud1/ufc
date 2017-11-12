library(rvest)
library(magrittr)
library(dplyr)


scrape_events <- function(){
  events_webpage <- read_html('http://www.fightmetric.com/statistics/events/completed?page=all')
  
  events_nodes <- html_nodes(events_webpage, ".b-statistics__table-events") 
  
  events_df <- 
    events_nodes %>%
    html_table(header = TRUE, fill = TRUE) %>%
    extract2(1) %>%
    na.omit
  
  events_df %>%
    mutate(url = html_nodes(events_nodes, "i a") %>% html_attr('href'))
}

# events_df <- scrape_events()