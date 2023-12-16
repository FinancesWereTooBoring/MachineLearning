# Get all important libraries and laod useful functions.
library(tidymodels)
library(tidyverse)
library(class)
library(skimr)
library(caret)
library(themis)
library(yardstick)
source("./helpful_functions.R")
load("./offers_censored.RData")
# Firstly we make splits

#prediction

# We need to set a seed
set.seed(666420)

# Final data set.
final_training_prediction_split <-
  offers |>
  make_appyear_split(test_year = 2023)


final_training <- training(final_training_prediction_split)

# Training data sets.
analysis_assessment_split <-
  offers |>
  censor_post_prediction_responses(years = 2022)|>
  drop_post_prediction_offers()|>
  filter(AppYear <= 2022) |>
  make_appyear_split(test_year = 2022)

analysis_train <- training(analysis_assessment_split)
assessment_test <- testing(analysis_assessment_split)

# The first method tried - knn.

#1. specify model
knn_model <-
  nearest_neighbor(neighbors = tune()) |>
  set_mode("classification") |>
  set_engine("kknn")

#2. recipe: normalize numeric to have mean = 0 
knn_recipe <-
  recipe(Status ~ ., 
         data = analysis_train) |>
  #feature engineering
  step_normalize(App4) |> #only integer value needed for step 5
  update_role(AppDate, OfferDate, ResponseDate, new_role = "id var") |>
  step_dummy(all_nominal_predictors()) |> 
  #removes predictors w 0 variance
  step_zv(all_predictors()) |>
  #counter for imbalance
  themis::step_downsample(Status)
knn_recipe

#3. create workflow object - combine model + recipe
knn_workflow <-
  workflow() |>
  add_model(knn_model) |>
  add_recipe(knn_recipe)
knn_workflow

# Set up a cross-validation control
set.seed(666420)
cv_folds_knn <- vfold_cv(analysis_train, v = 10, strata = "Status")

#4, set tuning grid 

knn_class_tune_grid <- tibble(neighbors = 1:50 * 2 + 1)
knn_class_tune_grid

#5. Tuning n nearest neighbors
#analysis <- validation_set(analysis_assessment_split)
knn_tune_results <-
  knn_workflow |>
  tune_grid(
    resamples = cv_folds_knn,
    grid = knn_class_tune_grid,
    metrics = metric_set(
      kap , f_meas, #all metrics as close to one as possible for good measure
      bal_accuracy, accuracy,
    )
  ) |>
  suppressWarnings()

#6. collect metrics 

knn_tune_metrics <-
  knn_tune_results |>
  collect_metrics()
knn_tune_metrics

#7. plot the metrics 
knn_tune_metrics |>
  ggplot(aes(x = neighbors, y = mean)) +
  geom_point() +
  geom_line() +
  facet_wrap(~.metric, scales = "free_y") +
  theme_bw()

#8. best neighbors 5 options ranked byt accuracy 
knn_tune_results |>
  show_best("bal_accuracy", n = 5) |>
  arrange(desc(mean), desc(neighbors))
#27 best neighbors: .726 | 17 --> winner smaller k less bias also more accurate measure 
#19 best neighbors: .718 | 10
#41 best neighbors: .727 | 20
#43 best neighbors: .729 | 50

#9. select best k neighbors - highest value for accuracy
knn_best_model <-
  knn_tune_results |>
  select_best(metric = "bal_accuracy")
knn_best_model

# 1SE rule 
knn_1se_model <- 
  knn_tune_results |> 
  select_by_one_std_err(metric = "bal_accuracy", desc(neighbors))
#10. finalize workflow
knn_workflow_final <-
  knn_workflow |>
  finalize_workflow(knn_best_model) #chosen based on best bc we want high performance
knn_workflow_final

#11. test on test set 

knn_class_last_fit <-
  knn_workflow_final |>
  last_fit(analysis_assessment_split,
           metrics = metric_set(
             accuracy, f_meas, kap, bal_accuracy 
           ),
           add_validation_set = TRUE
  )
knn_class_metrics <-
  knn_class_last_fit |>
  collect_metrics()
knn_class_metrics

#12. put in single frame 
knn_class_metrics <- knn_class_metrics |>
  select(.metric, .estimate) |>
  mutate(model = "knn_class")

#13. convert to comparable metrics 
conf_mat_knn <- knn_class_last_fit$.predictions[[1]] %>% conf_mat(truth = Status, estimate = .pred_class)

sensitivity_knn <- conf_mat_knn[1]$table %>% sensitivity()
precision_knn <- conf_mat_knn[1]$table %>% precision()
accuracy_knn <- conf_mat_knn[1]$table %>% accuracy()

# Lasso regression

# Model specification.
lasso_logistic_reg <- 
  logistic_reg(penalty = tune(), mixture = 1) |> 
  set_engine("glmnet")

# Recipe.
lasso_recipe <- 
  recipe(Status ~ ., data = analysis_train) |> 
  step_rm(AppDate, OfferDate,ResponseDate)|>
  step_dummy(all_nominal_predictors()) |> 
  step_zv(all_predictors()) |> 
  # normalize the predictors to have mean 0 and SD 1
  step_normalize(all_predictors()) |>
  step_downsample(Status)

# Set up workflow.
lasso_wf <- 
  workflow() |> 
  add_recipe(lasso_recipe) |> 
  add_model(lasso_logistic_reg)

set.seed(1810)
cv_folds_lasso <- vfold_cv(analysis_train, v = 10, strata = Status)

# Tune hyperparameters.
grid_lasso <- 
  grid_regular(penalty(range = c(-3, -1.5),
                       trans = log10_trans()), 
               levels = 100)
lasso_tune <- 
  lasso_wf |> 
  tune_grid(resamples = cv_folds_lasso, 
            grid = grid_lasso,
            metrics = metric_set(sensitivity, f_meas, kap, bal_accuracy))

lasso_tune_metrics <- 
  lasso_tune |> 
  collect_metrics()

lasso_tune_metrics |> 
 filter(.metric == "sensitivity") |> 
 ggplot(aes(x = penalty, y = mean, 
            ymin = mean - std_err, ymax = mean + std_err)) + 
 geom_pointrange(alpha = 0.5, size = .125) + 
 scale_x_log10() + 
 labs(y = "Sensitivity", x = expression(lambda)) +
 theme_bw()

# 1SE rule.
lasso_1se_model <- 
  lasso_tune |> 
  select_by_one_std_err(metric = "sensitivity", desc(penalty))

lasso_wf_tuned <- 
  lasso_wf |> 
  finalize_workflow(lasso_1se_model)

lasso_last_fit <- 
  lasso_wf_tuned |> 
  last_fit(analysis_assessment_split, metrics = metric_set(sensitivity, accuracy, precision, f_meas, kap, bal_accuracy))
lasso_test_metrics <- 
  lasso_last_fit |> 
  collect_metrics()
lasso_test_metrics <- 
  lasso_test_metrics |> 
  select(.metric, .estimate) |> 
  mutate(model = "lasso")

# Confusion matrix and the metrics.
conf_mat_lasso <- lasso_last_fit$.predictions[[1]] %>% conf_mat(truth = Status, estimate = .pred_class)

sensitivity_lasso <- conf_mat_lasso[1]$table %>% sensitivity()
precision_lasso <- conf_mat_lasso[1]$table %>% precision()
accuracy_lasso <- conf_mat_lasso[1]$table %>% accuracy()

# Random forest.

set.seed(221102)

# CV folds
cv_folds_rf <-
  analysis_train |>
  vfold_cv(v = 10, strata = Status)


# Defining the random forest recipe
rf_recipe_downsample <-
  recipe(Status ~., data = analysis_train) |>
  step_rm(AppDate, OfferDate, ResponseDate) |>
  update_role(AppDate, OfferDate, ResponseDate, new_role = "metadata") |>
  step_dummy(all_nominal_predictors()) |> 
  step_zv(all_predictors()) |>
  step_normalize(all_predictors()) |>
  themis::step_downsample(Status)

# Define the random forest model
rf_model_tune <-
  rand_forest(mtry = tune(), trees = 1000) |>
  set_mode("classification") |>
  set_engine("ranger", importance = "permutation")

# Setting up workflow.
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
  resamples = cv_folds_rf,
  grid = rf_tune_grid,
  metrics = class_metrics
)

# Plotting results for metrics

rf_tune_res |>
  collect_metrics() |>
  filter(.metric %in% c("sensitivity", "specificity")) |>
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

# Finishing workflow.

best_rf <- select_best(rf_tune_res, "sensitivity")
rf_final_wf <- finalize_workflow(rf_tune_wf, best_rf)

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

# Boosting model.
set.seed(221102)

cv_folds_boosting <- analysis_train |> vfold_cv(v = 10, strata = Status)

# Define recipe.
boosting_recipe <-
  recipe(Status ~ ., data = analysis_train) |>
  step_rm(AppDate, OfferDate, ResponseDate) |>
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |>
  themis::step_downsample(Status)

# Define the model.

boosting_model_tune <-
  boost_tree(
    trees = tune(),
    tree_depth = tune(),
    learn_rate = tune(),
    stop_iter = 400) |>
  set_mode("classification") |>
  set_engine("xgboost")

# Set up work workflow.

boosting_tune_wf <-
  workflow() |>
  add_recipe(boosting_recipe) |>
  add_model(boosting_model_tune)

# Tuning hyperparameters.

class_metrics <- metric_set(
  accuracy, kap, sensitivity,
  specificity, roc_auc)

num_cores <- parallel::detectCores()
num_cores

doParallel::registerDoParallel(cores = num_cores - 1L)


grid_max_entropy(trees(range = c(0, 10000)),
                 learn_rate(range = c(-2, -1)),
                 tree_depth(),
                 size = 10)#20

boosting_grid <- crossing(
  trees = 500 * 1:20,
  learn_rate = c(0.1, 0.01, 0.001),
  tree_depth = c(1, 2, 3))
boosting_grid

boosting_tune_res <- tune_grid(
  boosting_tune_wf,
  resamples = cv_folds_boosting,
  grid = boosting_grid,
  metrics = class_metrics)

# Getting metrics.

boosting_tune_metrics <-
  boosting_tune_res |>
  collect_metrics()

boosting_tune_metrics |>
  filter(.metric == "sensitivity") |>
  ggplot(aes(
    x = trees, y = mean,
    colour = factor(tree_depth)
  )) +
  geom_path() +
  labs(y = "Sensitivity") +
  scale_colour_manual(values = c("#D55E00", "#0072B2", "#009E73")) +
  facet_wrap(~learn_rate) +
  labs(colour = "tree_depth") +
  theme_bw() +
  theme(
    legend.position = c(.98, .98),
    legend.justification = c(1, 1),
    legend.background = element_rect(colour = "black")
  )

boosting_tune_metrics |>
  filter(learn_rate == 0.01 & tree_depth == 2 & trees <= 5000) |>
  select(trees:learn_rate, .metric, mean, std_err) |>
  filter(.metric %in% c("sensitivity", "specificity")) |>
  mutate(low = mean - std_err, high = mean + std_err) |>
  select(-std_err) |>
  pivot_wider(
    id_cols = trees:learn_rate,
    names_from = .metric,
    values_from = c(mean, low, high)
  ) |>
  select(trees, specificity = mean_specificity, ends_with("sensitivity")) |>
  ggplot() +
  aes(
    x = specificity,
    y = mean_sensitivity, ymin = low_sensitivity, ymax = high_sensitivity,
    colour = factor(trees, ordered = TRUE)
  ) +
  geom_pointrange() +
  geom_text(aes(label = trees), position = position_nudge(y = .01)) +
  scale_colour_viridis_d(begin = .3, end = .95, option = "E") +
  theme_bw() +
  labs(colour = "trees")

# Finalizing the workflow.

boosting_best <-
  boosting_tune_metrics |>
  filter(tree_depth == 2, learn_rate == 0.1, trees == 3000) |>
  distinct(trees, tree_depth, learn_rate)

boosting_final_wf <-
  finalize_workflow(boosting_tune_wf, boosting_best)

boosting_final_fit <-
  boosting_final_wf |>
  last_fit(analysis_assessment_split, metrics = class_metrics)

boosting_test_results <-
  boosting_final_fit |>
  collect_metrics()

predictions <- boosting_final_fit$.predictions[[1]]

# Confusion matrix.
confution_matrix<- predictions|>
  conf_mat(truth = Status, estimate = .pred_class)

sensitivity_Boosting <- confution_matrix[1]$table %>% sensitivity()
precision_Boosting <- confution_matrix[1]$table %>% precision()
accuracy_Boosting <- confution_matrix[1]$table %>% accuracy()

# Final model - lasso. 
# Loading the uncensored data.

lasso_last_fit |>
  augment() |>
  group_by(Program) |>
  summarise(
    Predicted_N = sum(.pred_Enrolled >= .5),
    Predicted_Prob = mean(.pred_Enrolled)
  )

load("data/offers_uncensored.RData")

lasso_final_model <- lasso_last_fit %>%
  last_fit(final_training_prediction_split, metrics = metric_set(sensitivity, precision))

lasso_final_test_metrics <- 
  lasso_final_model |> 
  collect_metrics()
lasso_final_test_metrics <- 
  lasso_final_test_metrics |> 
  select(.metric, .estimate) |> 
  mutate(model = "lasso")








