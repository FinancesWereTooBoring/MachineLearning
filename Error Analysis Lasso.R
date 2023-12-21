library(tidymodels)
library(tidyverse)
library(beepr)
library(yardstick)
library(themis)
source("./data_processing.R")

load("data/offers_uncensored.RData")
source("./helpful_functions.R")

# We need to set a seed
set.seed(666420)

final_training_prediction_split <-
  offers |>
  make_appyear_split(test_year = 2023)

#final_training_prediction_split <- initial_split(data = final_training_prediction_split, strata = Status)

final_training <- training(final_training_prediction_split)

analysis_assessment_split <-
  offers |>
  censor_post_prediction_responses(years = 2022)|>
  drop_post_prediction_offers()|>
  filter(AppYear <= 2022) |>
  make_appyear_split(test_year = 2022)

analysis_train <- training(analysis_assessment_split)
assessment_test <- testing(analysis_assessment_split)

loaded_lass <- readRDS("lasso_model_tuned_final.rds")
lasso_final_model <- loaded_lass %>%
  last_fit(final_training_prediction_split, metrics = metric_set(sensitivity, precision))

lasso_final_test_metrics <- 
  lasso_final_model |> 
  collect_metrics()
lasso_final_test_metrics <- 
  lasso_final_test_metrics |> 
  select(.metric, .estimate) |> 
  mutate(model = "lasso")

# extracting predictions
lasso_aug <-
  lasso_final_model |>
  augment()

lasso_aug |>
  select(Status, .pred_class)

error_metrics <- metric_set(rmse, mae, rsq_trad)

lasso_aug |>
  error_metrics(truth = Status, estimate = .pred_class)

library(caret)

conf_matrix <- confusionMatrix(lasso_aug$.pred_class, lasso_aug$Status)
conf_matrix
