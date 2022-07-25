# remotes::install_github("mlverse/torch")
# remotes::install_github("mlverse/tabnet")

library(tabnet)
library(xgboost)
library(tidyverse)
library(tidymodels)
library(vip)
library(skimr)
library(skimr)
library(timetk)
library(ranger)
library(kernlab)
tidymodels_prefer()

train <- read_csv("data/train.csv")
test <- read_csv("data/test.csv")

train <- train %>%
    select(-ends_with("PE"), -contains("C02"), -id)

test <- test %>%
    select(-ends_with("PE"), -contains("C02"))

# EDA
train %>% glimpse()
train %>% skim()

# SPLITS
set.seed(42)
splits <- initial_split(train)
vote_train <- training(splits)
vote_test <- testing(splits)
train_folds <- vfold_cv(train, v = 10)

# XGBOOST

xgb_spec <- boost_tree(
  trees = 1000,
  tree_depth = tune(),
  min_n = tune(),
  loss_reduction = tune(),
  sample_size = tune(),
  mtry = tune(),
  learn_rate = tune(),
) %>% 
  set_engine("xgboost") %>%
  set_mode("regression")
xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), train),
  learn_rate(),
  size = 30
)
xgb_wf <- workflow() %>%
  add_formula(percent_dem ~ .) %>%
  add_model(xgb_spec)
xgb_res <- tune_grid(
    xgb_wf,
    resamples = train_folds,
    grid = xgb_grid,
    control = control_grid(save_pred = TRUE)
  )

collect_metrics(xgb_res)

png(file="xgb_plot.png")
xgb_res %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  select(mean, mtry:sample_size) %>%
  pivot_longer(mtry:sample_size,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")
dev.off()

best_rmse <- select_best(xgb_res, "rmse")

final_xgb <- finalize_workflow(
  xgb_wf,
  best_rmse
)

# Main Arguments:
#   mtry = 89
#   trees = 1000
#   min_n = 22
#   tree_depth = 11
#   learn_rate = 0.0311801027418112
#   loss_reduction = 1.80758172653917e-07
#   sample_size = 0.425720357953105

final_res <- last_fit(final_xgb, splits)
collect_metrics(final_res)

final_fit <- final_xgb %>% fit(train)

predictions <- predict(final_fit, test) %>%
    bind_cols(test) %>%
    select(Id = id, Predicted = .pred)

write.csv(predictions, file = "xgb_fit.csv", row.names = FALSE)









# SVM RBF

svm_mod <- svm_rbf(
  cost = tune(), 
  rbf_sigma = tune() ) %>%
  set_mode("regression") %>%
  set_engine("kernlab")

svm_wf <- workflow() %>%
  add_formula(percent_dem ~ .) %>%
  add_model(svm_mod)

svm_rec <- recipe(percent_dem ~ ., data = train)

svm_search <-
  svm_wf %>% 
  tune_grid(
    percent_dem ~ .,
    resamples = train_folds
  )



rbf_res <- tune_grid(
    rbf_wf,
    resamples = train_folds,
    grid = rbf_grid,
    control = control_grid(save_pred = TRUE)
  )

rbf_fit <- rbf_wf %>% fit(train)



collect_metrics(xgb_res)




# LETS TRY RIDGE (SVR) kernel='linear'
# (SVR) kernel='rbf'
# ensemble regressors

# KRR_trainer with radial_basis_kernel
# SVR trainer with radial_basis_kernel OR histogram_intersection_kernel











tab_recipe <- recipe(percent_dem ~ ., vote_train)
tab_recipe %>% prep() %>% juice() %>% glimpse()
tab_flow <- workflow() %>%
    add_model(
        tabnet(
            mode = "regression",
            batch_size = 256,
            virtual_batch_size = 256,
            epochs = 20
        ) %>% 
            set_engine("torch", verbose = TRUE)
    ) %>%
    add_recipe(tab_recipe)
tab_fit <- tab_flow %>%
    fit(vote_train)
tab_fit %>%
    pull_workflow_fit() %>%
    vip()
predict(tab_fit, vote_test) %>%
    bind_cols(vote_test) %>%
    select(percent_dem, .pred) %>%
    metrics(truth = percent_dem, estimate = .pred)










set.seed(22)

elastic_tune_spec <-
  linear_reg(
    mixture = tune(),
    penalty = tune()
  ) %>% 
  set_engine("glmnet")
elastic_grid <- grid_regular(penalty(), mixture(), levels = 10)
elastic_wf <- workflow() %>%
  add_model(elastic_tune_spec) %>%
  add_formula(percent_dem ~ .)
elastic_res <-
  elastic_wf %>% 
  tune_grid(
    resamples = train_folds,
    grid = elastic_grid
  )
elastic_res %>% show_best("rmse")

collect_metrics(elastic_res)

final_elastic <- finalize_workflow(
  elastic_wf,
  select_best(elastic_res, "rmse")
)

# Main Arguments:
#   penalty = 0.0774263682681128
#   mixture = 0
  
final_elastic
final_res <- last_fit(final_elastic, splits)
collect_metrics(final_res)

final_fit <- final_elastic %>% fit(train)

predictions <- predict(final_fit, test) %>%
    bind_cols(test) %>%
    select(Id = id, Predicted = .pred)

write.csv(predictions, file = "elastic_fit.csv", row.names = FALSE)
