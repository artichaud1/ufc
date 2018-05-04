source('src/models/ranger/model_ranger_functions.R')

raw_df <- read.csv('data/fights_model.csv')

features_df <- make_features(raw_df)
rec <- make_recipe(features_df)
resamples_df <- make_resamples(features_df, rec)

param_grid <- make_params(mtry = 6:12, 
                          min.node.size = c(150, 175),
                          num.trees = c(200, 250))

tuning_results <- 
  grid_search(resamples_df, 
              'target', 
              param_grid, 
              train_predict_ranger,
              list(acc = accuracy),
              threshold = 0.50) %>%
  mutate(model = 'ranger')

tuning_results_summ <-
  tuning_results %>%
  group_by_at(names(transpose(param_grid))) %>%
  summarise(acc = mean(acc))

ggplot(data = tuning_results_summ,
       aes(x = mtry, y = acc, col = factor(min.node.size))) +
  facet_wrap(~ num.trees) + 
  geom_point() +
  geom_line() +
  theme_light()

best_paramset <- 
  tuning_results %>%
  group_by(paramset) %>%
  summarise(acc = mean(acc)) %>%
  filter(acc == max(acc)) %>%
  pull(paramset)

best_tuning_results <- 
  tuning_results %>%
  filter(paramset == best_paramset)

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
plot_roc(roc_results, 'recall', 'precision')

# Code below is relevant to visualize tuning results
#
# tuning_results_summ <- 
#   tuning_results %>%
#   group_by_at(vars(-id, -pred, -acc)) %>%
#   summarise(acc = mean(acc))
#   
# ggplot(data = tuning_results_summ,
#        aes(x = num.trees, y = acc, col = factor(min.node.size))) + 
#   geom_point() + 
#   geom_line() +
#   theme_light()


# Code below is relevant to train final model on train_df and test on test_df

# prepped_rec <- prep(rec, retain = TRUE)
# baked_train_df <- as.data.frame(juice(prepped_rec))
# baked_test_df <- bake(prepped_rec, test_df)
# 
# model <- train_ranger(baked_train_df, 
#                       target = 'target', 
#                       params = list(
#                         mtry = 6, 
#                         min.node.size = 200,
#                         num.trees = 100
#                       ))
# 
# preds <- predict_ranger(model, baked_test_df)
# 
# res_df <- 
#   bind_cols(target = as.numeric(as.character(baked_test_df[['target']])), predicted = preds)
# 
# accuracy(res_df$predicted, res_df$target)
# roc_data <- roc(res_df$predicted, res_df$target, model = 'ranger') 
# 
# roc_data %>% plot_roc('recall', 'precision')
