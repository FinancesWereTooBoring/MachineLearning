library(tidymodels)
library(tidyverse)
source("./data_processing.R")

lasso_reg <- linear_reg(penalty = tune(), mixture = 1) %>%  
  set_engine("glmnet")

linear_reg_recipe <- 
  recipe(Status ~ ., data = analysis_train) |> 
  step_dummy(all_nominal_predictors()) |> 
  step_dummy(all_outcomes()) |>
  # I still include interaction term
  step_interact(~ all_predictors():all_predictors()) |> 
  # remove any resulting variables that have only one value
  # and thus zero variance ("zv")
  step_zv(all_predictors()) |> 
  # normalize the predictors to have mean 0 and SD 1
  step_normalize(all_predictors())

