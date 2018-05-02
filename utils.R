library(purrr)
library(stringr)

make_params <- function(...){
  args <- list(...)
  
  param_grid <- 
    cross(args)
  
  param_ids <- format(seq_along(param_grid)) %>% str_replace(' ', '0')
  
  map2(
    param_grid,
    param_ids,
    ~ c(.x, paramset = .y)
  )
}
