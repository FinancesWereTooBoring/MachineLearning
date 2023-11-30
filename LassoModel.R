library(tidymodels)
library(tidyverse)
source("./data_processing.R")

linear_reg_recipe <- 
  recipe(Balance ~ ., data = Credit_train) |> 
  step_dummy(all_nominal_predictors()) |> 
  # I still include interaction term
  step_interact(~ all_predictors():all_predictors()) |> 
  # remove any resulting variables that have only one value
  # and thus zero variance ("zv")
  step_zv(all_predictors()) |> 
  # normalize the predictors to have mean 0 and SD 1
  step_normalize(all_predictors())

