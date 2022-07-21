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
tidymodels_prefer()

train <- read_csv("data/train_clean.csv")
test <- read_csv("data/test_clean.csv")
head(train)
head(test)
colnames(train)

train %>% glimpse()
train %>% skim()

set.seed(42)
splits <- initial_split(train, prop = 0.80)
train <- training(splits)
test <- testing(splits)

tab_recipe <- recipe(dem ~., train)
tab_recipe %>% prep() %>% juice() %>% glimpse()

tab_flow <- workflow() %>%
    add_model(
        tabnet(
            mode = "regression",
            batch_size = 128,
            virtual_batch_size = 128,
            epochs = 10
        ) %>% 
            set_engine("torch", verbose = TRUE)
    ) %>%
    add_recipe(tab_recipe)

tab_fit <- tab_flow %>%
    fit(train)

# serialization
write_rds(tab_fit, "saved/tabnet.rds")
torch::torch_save(tab_fit$fit$fit$fit$fit, "saved/torch_network")

# loading
tab_fit <- read_rds("saved/tabnet.rds")

xgboost_recipe <- recipe(dem ~., train)

xgboost_flow <- workflow() %>%
    add_model(
        boost_tree(mode = "regression") %>%
            set_engine("xgboost")
    ) %>%
    add_recipe(xgboost_recipe)

xgboost_fit <- xgboost_flow %>% fit(train)


write_rds(xgboost_fit, "saved/xgboost.rds")

predict(tab_fit, test) %>%
    bind_cols(test) %>%
    select(dem, .pred) %>%
    metrics(truth = dem, estimate = .pred)

predict(xgboost_fit, test) %>%
    bind_cols(test) %>%
    select(dem, .pred) %>%
    metrics(truth = dem, estimate = .pred)

tab_fit %>%
    pull_workflow_fit() %>%
    vip()

xgboost_fit %>%
    pull_workflow_fit() %>%
    vip()

head(train)
head(test)

kaggle <- read_csv("data/test_clean.csv")

predict(xgboost_fit, kaggle) %>%
    bind_cols(kaggle)

predictions <- predict(tab_fit, kaggle) %>%
    bind_cols(kaggle) %>%
    select(Id = Id, Predicted = .pred)

summary(predictions$Predicted)

predictions$Predicted = ifelse(predictions$Predicted > 1, 1, predictions$Predicted )
predictions$Predicted = ifelse(predictions$Predicted < 0, 0, predictions$Predicted )

write.csv(predictions, file = "tab_fit.csv", row.names = FALSE)
