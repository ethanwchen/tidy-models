library(conflicted)
library(tidymodels)
library(workflows)
library(tune)
library(readr)
library(forcats)
library(doFuture)
library(caret)
library(caretEnsemble)
library(patchwork)
library(tabnet)
library(xgboost)
library(dlookr)
library(tidyverse)
library(tidymodels)
library(vip)
library(skimr)
library(skimr)
library(timetk)
library(ranger)
library(kernlab)
library(tictoc)
library(rpart)
library(CORElearn)
library(caretEnsemble)
tidymodels_prefer()
set.seed(42)

train <- read_csv("train.csv")
test <- read_csv("test.csv")
test$percent_dem <- test$id
train <- rename(train, Id = "id")
test <- rename(test, Id = "id")
train <- rename(train, target = "percent_dem")
test <- rename(test, target = "percent_dem")

outliers <- c(57, 439, 461, 693, 863, 936, 1236, 1254, 1305, 1380, 1709, 1727, 1847, 2027, 2104, 2132, 2643, 2980)
train <- train[!train$Id %in% outliers, ]
train <- train[-c(1322), ] # remove NA

nTrain <- nrow(train)
nTest <- nrow(test)
vote_df <- rbind(train, test)

vote_df2 <- recipe(target ~ ., data = vote_df) %>%
  update_role(Id, new_role = "id_variable") %>%
  step_zv(all_predictors()) %>% 
  step_YeoJohnson(all_predictors()) %>%
  step_normalize(all_predictors()) %>%
  prep() %>%
  juice()

vote_training <- head(vote_df2, nTrain)
vote_testing_raw <- tail(vote_df2, nTest) %>% select(-target)

tmp_rec1 <- recipe(target ~ ., data = vote_training) %>%
    step_rm(Id) %>%
    step_zv(all_predictors()) %>%
    step_YeoJohnson(all_predictors()) %>%
    step_normalize(all_predictors())

tmp_rec2 <- recipe(target ~ ., data = vote_training) %>%
    update_role(Id, new_role = "id_variable") %>%
    step_zv(all_predictors()) %>%
    step_YeoJohnson(all_predictors()) %>%
    step_normalize(all_predictors())

vote_rec1 <- prep(tmp_rec1)
vote_rec2 <- prep(tmp_rec2)
vote_juiced <- juice(vote_rec1)
vote_testing <- bake(vote_rec2, vote_testing_raw)

set.seed(42)
trControl <- trainControl(
    method = "cv",
    savePredictions = "final",
    index = createMultiFolds(vote_juiced$target, k = 10, times = 2),
    allowParallel = TRUE,
    verboseIter = TRUE
)

# model 1 (xgboost)
xgbTreeGrid <- expand.grid(nrounds = 2000, max_depth = 4, eta = 0.02,
    gamma = 0, colsample_bytree = 0.625, subsample = 0.5, min_child_weight = 4)
# model 2 (svmRadial)
svmGrid <- expand.grid(sigma = 0.0008, C = 100)
# model 3 (glmnet)
glmnetGrid <- expand.grid(alpha = 1.0, lambda = 1.232847e-04)
# model 4 (ranger)
rfGrid <- expand.grid(mtry = 64, splitrule = "variance", min.node.size = 2)
# model 5 (elastic)
enetGrid <- expand.grid(fraction = 1, lambda = 0.05555556)
# model 6 (bayseianRidge): NA
# model 7 (randomGLM): NA
# model 8 (gaussprRadial):
gauss_Grid <- expand.grid(sigma = 0.05)
# model 9 (krlsRadial): NA
set.seed(42)
stacked_model <- caretList(
    target ~ ., data = vote_juiced,
    trControl = trControl,
    metric = "RMSE",
    tuneList = list(
        xgb = caretModelSpec(method = "xgbTree", tuneGrid = xgbTreeGrid),
        svm = caretModelSpec(method = "svmRadial", tuneGrid = svmGrid),
        glmnet = caretModelSpec(method = "glmnet", tuneGrid = glmnetGrid),
        rf = caretModelSpec(method = "ranger", tuneGrid = rfGrid),
        enet = caretModelSpec(method = "enet", tuneGrid = enetGrid),
        # bayeRidge = caretModelSpec(method = "blassoAveraged"),
        gauss = caretModelSpec(method = "gaussprRadial", tuneGrid = gauss_Grid)
        # krls = caretModelSpec(method = "krlsRadial", tuneGrid = krls_Grid)
    )
)

# saveRDS(stacked_model, "stacked_model.rds")
# stacked_model <- readRDS("submissions/stacked_model.rds")

set.seed(42)
vote_stack <- caretEnsemble(stacked_model)
vote_pred <- predict(vote_stack, newdata = vote_testing) %>%
    bind_cols(vote_testing) %>%
    select(Id = Id, Predicted = ...1)
write.csv(vote_pred, file = "dalle.csv", row.names = FALSE)