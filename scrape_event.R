library(rvest)
library(stringr)
library(purrr)
library(tidyr)


scrape_event <- function(url){

  fights_webpage <- read_html(url)
  
  fights_nodes <- html_nodes(fights_webpage, '.b-fight-details__table')
  
  fights_df <- 
    fights_nodes %>%
    html_table(header = TRUE, fill = TRUE) %>%
    extract2(1) %>%
    na.omit
  
  # We re-extract fighter names into a vector
  fighters <- 
    fights_nodes %>%
    html_nodes('a.b-link') %>%
    html_text %>%
    map_chr(str_trim)
  
  # fighter1 are the odds, fighter2 are the evens
  fighter1 <- fighters[(1:length(fighters)) %% 2 == 1]
  fighter2 <- fighters[(1:length(fighters)) %% 2 == 0]
  
  fights_df %<>% 
    mutate(Fighter1 = fighter1, Fighter2 = fighter2) %>%
    select(-Fighter)
  
  remove_cols <- c('Str', 'Td', 'Sub', 'Pass')
  
  fights_df %<>% select(-one_of(remove_cols))
  
  fights_df %<>% 
    mutate(
      Fight_url = fights_nodes %>%
                    html_nodes('tbody tr.b-fight-details__table-row') %>% 
                    html_attr('data-link')
    )
  
  fights_df %>% na.omit
}

# View(scrape_fight('http://www.fightmetric.com/event-details/d856a0080ac09ed7'))

  
  
