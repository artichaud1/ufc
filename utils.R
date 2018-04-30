library(dplyr)

comb_df <- function(df1, df2){
  
  rows <- expand.grid(df1 = 1:nrow(df1), df2 = 1:nrow(df2))
  bind_cols(df1[rows$df1, ], df2[rows$df2, ])
  
}
