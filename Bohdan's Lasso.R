library(tidymodels)
library(tidyverse)
library(beepr)
library(yardstick)
source("./data_processing.R")
#loaded_lass <- readRDS("lasso_model_tuned.rds")
analysis_train$Status %>% table()

lasso_logistic_reg <- 
  logistic_reg(penalty = tune(), mixture = 1) |> 
  set_engine("glmnet")

lasso_recipe <- 
  recipe(Status ~ ., data = analysis_train) |> 
  step_rm(AppDate, OfferDate,ResponseDate)|>
  step_dummy(all_nominal_predictors()) |> 
  # I still include interaction terms
  #step_interact(~ all_predictors():all_predictors()) |> 
  # remove any resulting variables that have only one value
  # and thus zero variance ("zv")
  step_zv(all_predictors()) |> 
  # normalize the predictors to have mean 0 and SD 1
  step_normalize(all_predictors()) |>
  themis::step_downsample(Status)


lasso_wf <- 
  workflow() |> 
  add_recipe(lasso_recipe) |> 
  add_model(lasso_logistic_reg)

set.seed(1810)
cv_folds <- vfold_cv(analysis_train, v = 10, strata = Status) #I have no clue if it's needed

grid_lasso <- 
  grid_regular(penalty(range = c(-4.5, -1.5),
                       trans = log10_trans()), 
               levels = 100)
lasso_tune <- 
  lasso_wf |> 
  tune_grid(resamples = cv_folds, 
            grid = grid_lasso,
            metrics = metric_set(accuracy, f_meas, kap, bal_accuracy, sensitivity, precision, specificity))
#Well, at least it works now. But we`ll need to discuss which metrics to take.
beepr::beep()

lasso_tune_metrics <- 
  lasso_tune |> 
  collect_metrics()

lasso_tune_metrics |> 
  filter(.metric == "accuracy") |> 
  ggplot(aes(x = penalty, y = mean, 
             ymin = mean - std_err, ymax = mean + std_err)) + 
  geom_pointrange(alpha = 0.5, size = .125) + 
  scale_x_log10() + 
  labs(y = "Accuracy", x = expression(lambda)) +
  theme_bw()

# Assume that the correct model is already chosen
lasso_1se_model <- 
  lasso_tune |> 
  select_by_one_std_err(metric = "sensitivity", desc(penalty))

lasso_wf_tuned <- 
  lasso_wf |> 
  finalize_workflow(lasso_1se_model)

#saveRDS(lasso_wf_tuned, "lasso_model_tuned.rds")

# I guess last fit?
lasso_last_fit <- 
   lasso_wf_tuned |> 
   last_fit(analysis_assessment_split, metrics = metric_set(accuracy, f_meas, kap, bal_accuracy, sensitivity, precision, specificity))
 
lasso_test_metrics <- lasso_last_fit |> 
  collect_metrics()

lasso_last_fit |>
  collect_predictions() |>
  roc_curve(Status, .pred_Enrolled) |>
  autoplot()

 lasso_test_metrics <- 
   lasso_test_metrics |> 
   select(.metric, .estimate) |> 
   mutate(model = "lasso")
 
 lasso_last_fit |>
   extract_fit_parsnip() |>
   tidy() |>
   arrange(desc(abs(estimate)))
# output
# .metric      .estimate model
# <chr>            <dbl> <chr>
# 1 accuracy         0.961 lasso
# 2 f_meas           0.968 lasso
# 3 kap              0.919 lasso
# 4 bal_accuracy     0.967 lasso
#Rn't those sexy asf?

lasso_last_fit$.predictions[[1]] %>% conf_mat(truth = Status, estimate = .pred_class)

conf_mat_lasso[1]$table %>% sensitivity()
conf_mat_lasso[1]$table %>% precision()
conf_mat_lasso[1]$table %>% accuracy()
conf_mat_lasso[1]$table %>% specificity()

# metrics
# 1 sensitivity binary      0.945
# 1 precision binary        0.993
# 1 accuracy binary         0.961
# 1 specificity binary      0.989
# Work with final data set, in case we want to use lasso

lasso_final_model <- lasso_wf_tuned %>%
  last_fit(final_training_prediction_split)

lasso_final_model |>
  augment() |>
  group_by(Program) |>
  summarise(
    Predicted_N = sum(.pred_Enrolled >= 0.3),
    Predicted_Prob = mean(.pred_Enrolled)
  )
lasso_final_model[5]$.predictions
