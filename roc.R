
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