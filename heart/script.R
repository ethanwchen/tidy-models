library(tidyverse)
library(tidymodels)
library(stacks)
library(ggplot2)
library(xgboost)

train <- read_csv("heart_train.csv")
# which(train$thal == '?')
train$thal[6] = NA
train$thal[109] = NA
train$num <- as.factor(train$num)
train$sex <- as.factor(train$sex)
train$cp <- as.factor(train$cp)
train$fbs <- as.factor(train$fbs)
train$restecg <- as.factor(train$restecg)
train$exang <- as.factor(train$exang)
train$slope <- as.factor(train$slope)
train$thal <- as.factor(train$thal)
train$ca <- as.numeric(train$ca)

test <- read_csv("heart_test.csv")
test[c(42, 43), 1] = c(156, 150)
test$sex <- as.factor(test$sex)
test$cp <- as.factor(test$cp)
test$fbs <- as.factor(test$fbs)
test$restecg <- as.factor(test$restecg)
test$exang <- as.factor(test$exang)
test$slope <- as.factor(test$slope)
test$thal <- as.factor(test$thal)
test$ca <- as.numeric(test$ca)

set.seed(49)
heart_folds <- vfold_cv(train, v = 10, repeats = 2)
heart_rec <- recipe(num ~ ., data = train) %>%
    step_impute_knn(all_predictors(), neighbors = 5) %>%
    update_role(id, new_role = "id_variable") %>%
    step_dummy(sex, cp, fbs, restecg, exang, slope, thal) %>%
    step_zv(age, trestbps, chol, thalach, oldpeak, ca) %>%
    step_YeoJohnson(age, trestbps, chol, thalach, oldpeak, ca) %>%
    step_normalize(age, trestbps, chol, thalach, oldpeak, ca)
heart_wflow <- workflow() %>% add_recipe(heart_rec)
ctrl_grid <- control_stack_grid()

rf_spec <- 
  rand_forest(
    mtry = tune(),
    min_n = tune(),
    trees = 500
  ) %>%
  set_mode("classification") %>%
  set_engine("ranger")
rf_wflow <-
  heart_wflow %>%
  add_model(rf_spec)
rf_res <- 
  tune_grid(
    object = rf_wflow,
    resamples = heart_folds,
    grid = 10,
    control = ctrl_grid
  )

nnet_spec <-
  mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) %>%
  set_engine("nnet") %>%
  set_mode("classification")
nnet_rec <- 
  heart_rec %>% 
  step_normalize(all_predictors())
nnet_wflow <- 
  heart_wflow %>%
  add_model(nnet_spec)
nnet_res <-
  tune_grid(
    object = nnet_wflow,
    resamples = heart_folds,
    grid = 10,
    control = ctrl_grid
  )

xgb_spec <- 
  boost_tree(mtry = tune(), tree_depth = tune()) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification") 
xgb_wflow <- 
  heart_wflow %>%
  add_model(xgb_spec)
xgb_res <-
  tune_grid(
    object = xgb_wflow,
    resamples = heart_folds,
    grid = 10,
    control = ctrl_grid
  )

knn_spec <- 
  nearest_neighbor(neighbors = tune(), weight_func = "gaussian", dist_power = tune()) %>%
  set_engine("kknn") %>% 
  set_mode("classification")
knn_wflow <- 
  heart_wflow %>%
  add_model(knn_spec)
knn_res <-
  tune_grid(
    object = knn_wflow,
    resamples = heart_folds,
    grid = 10,
    control = ctrl_grid
  )

heart_stack <- 
  stacks() %>%
  add_candidates(rf_res) %>%
  add_candidates(nnet_res) %>%
  add_candidates(xgb_res) %>%
  add_candidates(knn_res) %>%
  blend_predictions() %>%
  fit_members()

heart_pred <- predict(heart_stack, test) %>%
    bind_cols(test) %>%
    select(id = id, Predicted = .pred_class)
write.csv(heart_pred, file = "best.csv", row.names = FALSE)
