install.packages("beepr")
library(tidymodels)
library(tidyverse)
library(beepr)
source("./data_processing.R")

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
  step_normalize(all_predictors())


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
            metrics = metric_set(accuracy, f_meas, kap, bal_accuracy))
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
  select_by_one_std_err(metric = "accuracy", desc(penalty))

lasso_wf_tuned <- 
  lasso_wf |> 
  finalize_workflow(lasso_1se_model)

# I guess last fit?
lasso_last_fit <- 
  lasso_wf_tuned |> 
  last_fit(analysis_assessment_split, metrics = metric_set(accuracy, f_meas, kap, bal_accuracy))
lasso_test_metrics <- 
  lasso_last_fit |> 
  collect_metrics()
lasso_test_metrics <- 
  lasso_test_metrics |> 
  select(.metric, .estimate) |> 
  mutate(model = "lasso")
# output
# .metric      .estimate model
# <chr>            <dbl> <chr>
#   1 accuracy         0.991 lasso
# 2 f_meas           0.993 lasso
# 3 kap              0.982 lasso
# 4 bal_accuracy     0.990 lasso
#Rn't those sexy asf?





