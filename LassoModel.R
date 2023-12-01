library(tidymodels)
library(tidyverse)
library(beepr)
source("./data_processing.R")

lasso_logistic_reg <- 
  logistic_reg(penalty = tune(), mixture = 1) |> 
  set_engine("glmnet")

lasso_recipe <- 
  recipe(Status ~ ., data = analysis_train) |> 
  step_rm(AppDate, OfferDate,ResponseDate)|>
  step_dummy(all_nominal_predictors()) |> 
  step_dummy(all_outcomes()) |>
  # I still include interaction terms
  #step_interact(~ all_predictors():all_predictors()) |> 
  # remove any resulting variables that have only one value
  # and thus zero variance ("zv")
  step_zv(all_predictors()) |> 
  # normalize the predictors to have mean 0 and SD 1
  step_normalize(all_predictors())


lasso_wf <- 
  workflow() |> 
  add_recipe(linear_reg_recipe) |> 
  add_model(lasso_reg)

set.seed(1810)
cv_folds <- vfold_cv(analysis_train, v = 10) #I have no clue if it's needed

grid_lasso <- grid_regular(penalty(c(1, 4), trans = log10_trans()), 
                           levels = 40)
lasso_tune <- 
  lasso_wf %>%  
  tune_grid(resamples = cv_folds, 
            grid = grid_lasso,
            metrics = metric_set(rmse, rsq_trad, mae))
beepr::beep()

lasso_tune_metrics <- 
  lasso_tune %>%  
  collect_metrics()
lasso_tune_metrics |> 
  filter(.metric == "rmse") |> 
  ggplot(aes(x = penalty, y = mean, 
             ymin = mean - std_err, ymax = mean + std_err)) + 
  geom_pointrange(alpha = 0.5) + 
  scale_x_log10() + 
  labs(y = "RMSE", x = expression(lambda)) +
  theme_bw()

