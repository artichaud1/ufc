

accuracy <- function(predicted, target, threshold = 0.5){
  mean(ifelse(predicted > threshold, 1, 0) == target)
} 