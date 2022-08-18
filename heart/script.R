library(tidyverse)
library(tidymodels)
library(stacks)
library(ggplot2)
library(xgboost)

# NOTE: We are reproduce the same values in our submission “second.csv” 
# that was submitted on July 26th. This recording provides proof of our
# ability to recreate this submission. It seems to only work on a MacOS system, 
# similar to what happened to us in the first Kaggle competition. 
# We included a recording, showing our final CSV, to prove reproducibility. 


train <- read_csv("heart_train.csv")
# which(train$thal == '?')
# replace question marks with NA
train$thal[6] = NA
train$thal[109] = NA

# convert into factors
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

# create training split
set.seed(49)
heart_folds <- vfold_cv(train, v = 10, repeats = 2)

# create recipe for all the models
heart_rec <- recipe(num ~ ., data = train) %>%
    step_impute_knn(all_predictors(), neighbors = 5) %>%
    update_role(id, new_role = "id_variable") %>%
    step_dummy(sex, cp, fbs, restecg, exang, slope, thal) %>%
    step_zv(age, trestbps, chol, thalach, oldpeak, ca) %>%
    step_YeoJohnson(age, trestbps, chol, thalach, oldpeak, ca) %>%
    step_normalize(age, trestbps, chol, thalach, oldpeak, ca)
heart_wflow <- workflow() %>% add_recipe(heart_rec)
ctrl_grid <- control_stack_grid()

# create rf tuning grid with ranger
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

# create nn tuning grid with nnet
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

# create xgb tuning grid with xgb_spec
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

# create knn tuning grid with kknn
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

# create ensemble method of all 4 models
heart_stack <- 
  stacks() %>%
  add_candidates(rf_res) %>%
  add_candidates(nnet_res) %>%
  add_candidates(xgb_res) %>%
  add_candidates(knn_res) %>%
  blend_predictions() %>%
  fit_members()

# create predictions and write to second.csv
heart_pred <- predict(heart_stack, test) %>%
    bind_cols(test) %>%
    select(id = id, Predicted = .pred_class)
write.csv(heart_pred, file = "best.csv", row.names = FALSE)
