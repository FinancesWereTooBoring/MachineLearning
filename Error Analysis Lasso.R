library(tidymodels)
library(tidyverse)
library(beepr)
library(yardstick)
library(themis)
source("./data_processing.R")

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