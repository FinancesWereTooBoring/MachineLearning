library(tidymodels)
library(tidyverse)
library(skimr)


set.seed(221102)

# CV folds
cv_folds <-
  analysis_train |>
  vfold_cv(v = 10, strata = Status)
  
skim(analysis_train)

# Defining the random forest model
rf_recipe_downsample <-
  recipe(Status ~., data = analysis_train) |>
  step_rm(AppDate, OfferDate,ResponseDate) |>
  update_role(AppDate, OfferDate,ResponseDate, new_role = "metadata") |>
  step_dummy(all_nominal_predictors()) |> 
  step_zv(all_predictors()) |>
  step_normalize(all_predictors())

  #themsis::step_downsample(Status)
# need to double check if this is necessary

rf_recipe_downsample

# Define the random forest model
rf_model_tune <-
  rand_forest(mtry = tune(), trees = 1000) |>
  set_mode("classification") |>
  set_engine("ranger", importance = "permutation")

rf_tune_wf <- 
  workflow() |>
  add_recipe(rf_recipe_downsample) |>
  add_model(rf_model_tune)

class_metrics <- metric_set(
  accuracy, kap, sensitivity,
  specificity, roc_auc
)

# Tuning the model
rf_tune_grid <- grid_regular(mtry(range = c(1, 17)), levels = 17)

num_cores <- parallel::detectCores()
num_cores

doParallel::registerDoParallel(cores = num_cores - 1L)

set.seed(231164)
rf_tune_res <- tune_grid(
  rf_tune_wf,
  resamples = cv_folds,
  grid = rf_tune_grid,
  metrics = class_metrics
)

# Unable to do randomforest with missing data in ResponseDate

# Plotting results for metrics


