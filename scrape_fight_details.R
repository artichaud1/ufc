library(rvest)
library(magrittr)
library(dplyr)


scrape_fight_details <- function(url){
  
  webpage <- read_html(url)
  
  # scrape Totals table: 2nd section
  totals <- 
    webpage %>%
    html_nodes('.b-fight-details') %>%
    html_nodes('section') %>%
    extract2(2) %>%
    html_nodes('table tbody tr td p') %>%
    html_text()
  
  totals_labels <-
    c('Fighter', 'Kd', 'SigStrikes', 'SigStrikesPerc', 'TotStrikes', 'Td', 'TdPerc', 'SubAtt', 'Pass', 'Rev')
  
  totals_fighter1 <- as.list(totals[(1:length(totals)) %% 2 == 1] )
  names(totals_fighter1) <- paste0(totals_labels, '1')
  totals_fighter2 <- as.list(totals[(1:length(totals)) %% 2 == 0] )
  names(totals_fighter2) <- paste0(totals_labels, '2')
  
  totals <- cbind(as_data_frame(totals_fighter1), as_data_frame(totals_fighter2))
  
  # remove redundant information from totals table
  totals <- totals[, !(colnames(totals) %in% 
                         c('Fighter1', 'Fighter2',
                           'SigStrikes1', 'SigStrikes2',
                           'Td1', 'Td2',
                           'SubAtt1', 'SubAtt2'))]
  
  # scrape Significant Strikes table: the only direct-child table
  sig_strikes <- 
    webpage %>%
    html_nodes('.b-fight-details > table') %>%
    html_nodes('tbody tr td p') %>%
    html_text()
  
  sig_strikes_labels <-
    c('Fighter', 'SigStrikes', 'SigStrikesPerc', 'Head', 'Body', 'Leg', 'Distance', 'Clinch', 'Ground')
  
  sig_strikes_fighter1 <- as.list(sig_strikes[(1:length(sig_strikes)) %% 2 == 1] )
  names(sig_strikes_fighter1) <- paste0(sig_strikes_labels, '1')
  sig_strikes_fighter2 <- as.list(sig_strikes[(1:length(sig_strikes)) %% 2 == 0] )
  names(sig_strikes_fighter2) <- paste0(sig_strikes_labels, '2')
  
  sig_strikes <- cbind(as_data_frame(sig_strikes_fighter1), as_data_frame(sig_strikes_fighter2))
  
  # remove redundant info from sig_strikes table
  sig_strikes <- sig_strikes[, !(colnames(sig_strikes) %in% 
                                 c('Fighter1', 'Fighter2', 
                                   'SigStrikes1', 'SigStrikes2',
                                   'SigStrikesPerc1', 'SigStrikesPerc2'))]
  
  cbind(totals, sig_strikes)
}

# View(scrape_fight_details('http://www.fightmetric.com/fight-details/b6b5ec324449befd'))