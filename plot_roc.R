library(ggplot2)
library(plotly)

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