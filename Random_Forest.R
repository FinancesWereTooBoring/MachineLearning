library(tidymodels)
library(tidyverse)
library(skimr)
library(beepr)
library(themis)

set.seed(221102)


# CV folds
cv_folds <-
  analysis_train |>
  vfold_cv(v = 10, strata = Status)
  
skim(analysis_train)

# Defining the random forest model
rf_recipe_downsample <-
  recipe(Status ~., data = analysis_train) |>
  step_rm(AppDate, OfferDate, ResponseDate) |>
  update_role(AppDate, OfferDate, ResponseDate, new_role = "metadata") |>
  step_dummy(all_nominal_predictors()) |> 
  step_zv(all_predictors()) |>
  step_normalize(all_predictors()) |>
  themis::step_downsample(Status)

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
  accuracy, sensitivity, specificity, precision,
  kap, roc_auc)

# Tuning the model
rf_tune_grid <- grid_regular(mtry(range = c(1, 14)), levels = 14)
rf_tune_grid

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

beepr::beep()

# Plotting results for metrics

rf_tune_res |>
  collect_metrics() |>
  filter(.metric %in% c("sensitivity", "precision")) |>
  ggplot(aes(
    x = mtry, y = mean, ymin = mean - std_err,
    ymax = mean + std_err,
    colour = .metric
  )) +
  geom_errorbar() +
  geom_line() +
  geom_point() +
  scale_colour_manual(values = c("#D55E00", "#0072B2")) +
  facet_wrap(~.metric, ncol = 1, scales = "free_y") +
  guides(colour = "none") +
  theme_bw()

rf_tune_res |>
  collect_metrics() |>
  filter(.metric %in% c("roc_auc", "accuracy", "kap")) |>
  ggplot(aes(
    x = mtry, y = mean, ymin = mean - std_err,
    ymax = mean + std_err, colour = .metric
  )) +
  geom_errorbar() +
  geom_line() +
  geom_point() +
  scale_colour_manual(values = c("#D55E00", "#0072B2", "#009E73")) +
  facet_wrap(~.metric, ncol = 1, scales = "free_y") +
  guides(colour = "none") +
  theme_bw()

best_rf <- select_best(rf_tune_res, "sensitivity")
rf_final_wf <- finalize_workflow(rf_tune_wf, best_rf)
rf_final_wf

# Test set
set.seed(666420)
rf_final_fit <-
  rf_final_wf |>
  last_fit(analysis_assessment_split, metrics = class_metrics)

rf_final_fit |>
  collect_metrics()

rf_final_fit |>
  collect_predictions() |>
  conf_mat(truth = Status, estimate = .pred_class)

rf_final_fit |>
  collect_predictions() |>
  roc_curve(Status, .pred_Enrolled) |>
  autoplot()

#.metric     .estimator .estimate .config             
#<chr>       <chr>          <dbl> <chr>               
#1 accuracy    binary         0.949 Preprocessor1_Model1
#2 kap         binary         0.893 Preprocessor1_Model1
#3 sensitivity binary         0.925 Preprocessor1_Model1
#4 specificity binary         0.987 Preprocessor1_Model1
#5 roc_auc     binary         0.980 Preprocessor1_Model1

# Working with final data set
rf_final_model <- rf_final_wf %>%
  last_fit(final_training_prediction_split)

beepr::beep()

rf_final_model |>
  augment() |>
  group_by(Program) |>
  summarise(
    Predicted_N = sum(.pred_Enrolled >= .3),
    Predicted_Prob = mean(.pred_Enrolled)
  )

rf_final_model[5]$.predictions