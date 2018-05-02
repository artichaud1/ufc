library(ggplot2)
library(plotly)
library(foreach)
library(dplyr)


roc <- function(pred, target, step = 0.01, model = 'model'){
  
  data <- data.frame(pred = pred, target = target)
  
  thresholds <- seq(0, 1, by = step)
  
  foreach::foreach(threshold = thresholds, .combine = bind_rows, .multicombine = TRUE) %:% 
    foreach::when(threshold >= 0 && threshold <= 1) %do% {
    
      data %>%
      mutate(pred = map_int(pred, ~ ifelse(.x >= threshold, 1L, 0L))) %>%
      summarise(
        recall = sum(target == 1 & pred == 1) / sum(target == 1),
        precision = sum(target == 1 & pred == 1) / sum(pred == 1),
        fpr = sum(target == 0 & pred == 1) / sum(target == 0)
      ) %>%
      mutate(
        tpr = recall,
        tnr = 1 - fpr,
        threshold = threshold,
        model = model
      )
  } %>%
    as_tibble()
}

plot_roc <- function(data, stat_x = 'fpr', stat_y = 'tpr'){
  
  g <- 
    ggplot(data = data, aes_string(x = stat_x, y = stat_y, text = 'threshold')) +
    geom_abline(intercept = 0, slope = 1, color = "white") +
    geom_line(aes(color = model)) + 
    labs(color = "Model") + 
    ggtitle("ROC Chart") +
    scale_x_continuous(name = stat_x, limits = c(0, 1), breaks = seq(0, 1, by=0.1)) +
    scale_y_continuous(name = stat_y, limits = c(0, 1), breaks = seq(0, 1, by=0.1))
  
  ggplotly(g, width=800, height=600) %>%
    config(displayModeBar = F) %>%
    layout(legend = list(x = 100, y = 100))
}