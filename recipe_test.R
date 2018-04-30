train_df = data_frame(x1 = runif(100), 
                x2 = sample(c('a1', 'a2'), 100, replace = TRUE), 
                y = 2*x1 + ifelse(x2=='a1', 3, -5) + runif(5))

test_df = data_frame(x1 = 0.5, x2 = 'd')

lm_mod <- lm(y ~ ., data = train_df)
predict(lm_mod, test_df)

rec <- 
  recipe(train_df) %>%
  add_role(y, new_role = 'outcome') %>%
  add_role(x1, x2, new_role = 'predictor') %>%
  step_novel(x2) %>%
  step_dummy(x2) %>%
  step_zv(all_predictors()) %>%
  prep(retain = TRUE)

juice(rec)

bake(rec, test_df)

lm_mod <- lm(y ~ ., data = juice(rec))
summary(lm_mod)
predict(lm_mod, bake(rec, test_df))
