

best_paramsets <- 
  tuning_results %>%
  group_by(model, paramset) %>%
  summarise(acc = mean(acc)) %>%
  filter(acc == max(acc)) %>%
  select(model, paramset)

best_tuning_results <- 
  tuning_results %>%
  inner_join(best_paramsets, by = c('model', 'paramset'))

roc_results <- 
  best_tuning_results %>%
  mutate(
    roc = map(
      pred,
      ~ roc(.$predicted, .$target)
    )
  ) %>%
  unnest(roc) %>%
  group_by(model, threshold) %>%
  summarise(
    precision = mean(precision),
    recall = mean(recall),
    tpr = mean(tpr),
    fpr = mean(fpr)
  )

plot_roc(roc_results, 'fpr', 'tpr')