library(tidyverse)
library(tidymodels)

load("offers_censored.RData")
source("./helpful_functions.R")

# We need to set a seed
set.seed(666420)

final_training_prediction_split <-
  offers |>
  make_appyear_split(test_year = 2023)



final_training <- training(final_training_prediction_split)

analysis_assessment_split <-
  offers |>
  censor_post_prediction_responses(years = 2022)|>
  drop_post_prediction_offers()|>
  filter(AppYear <= 2022) |>
  make_appyear_split(test_year = 2022)

analysis_train <- training(analysis_assessment_split)
assessment_test <- testing(analysis_assessment_split)

#
cv_folds <- analysis_train |> vfold_cv(v = 10, strata = Status)


boosting_recipe <-
  recipe(Status ~ ., data = analysis_train) |>
  step_rm(AppDate, OfferDate, ResponseDate) |>
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |>
  themis::step_downsample(Status)

boosting_model_tune <-
  boost_tree(
    trees = tune(),
    tree_depth = tune(),
    learn_rate = tune(),
    stop_iter = 400) |>
  set_mode("classification") |>
  set_engine("xgboost")

boosting_tune_wf <-
  workflow() |>
  add_recipe(boosting_recipe) |>
  add_model(boosting_model_tune)
boosting_tune_wf

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
  resamples = cv_folds,
  grid = boosting_grid,
  metrics = class_metrics)

#####

boosting_tune_metrics <-
  boosting_tune_res |>
  collect_metrics()
boosting_tune_metrics

boosting_tune_metrics |>
  filter(.metric == "sensitivity") |>
  ggplot(aes(
    x = trees, y = 1 - mean,
    colour = factor(tree_depth)
  )) +
  geom_path() +
  labs(y = "Misclassification rate") +
  scale_colour_manual(values = c("#D55E00", "#0072B2", "#009E73")) +
  facet_wrap(~learn_rate, labeller = label_both) +
  labs(colour = "tree_depth") +
  theme_bw() +
  theme(
    legend.position = c(.98, .98),
    legend.justification = c(1, 1),
    legend.background = element_rect(colour = "black")
  )

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
  filter(.metric == "specificity") |>
  ggplot(aes(
    x = trees, y = mean,
    colour = factor(tree_depth)
  )) +
  geom_path() +
  labs(y = "Specificity") +
  scale_colour_manual(values = c("#D55E00", "#0072B2", "#009E73")) +
  facet_wrap(~learn_rate) +
  labs(colour = "tree_depth") +
  theme_bw() +
  theme(
    legend.position = c(.98, .02),
    legend.justification = c(1, 0),
    legend.background = element_rect(colour = "black")
  )
#######



boosting_tune_metrics |>
  filter(.metric %in% c("sensitivity", "percision")) |>
  ggplot(aes(x = trees, y = mean, colour = .metric)) +
  geom_path() +
  facet_grid(learn_rate ~ tree_depth, labeller = label_both) +
  scale_colour_manual(values = c("#D55E00", "#0072B2")) +
  theme_bw() +
  labs(y = NULL) +
  theme(
    legend.position = c(.98, .2),
    legend.justification = c(1, 0),
    legend.background = element_rect(colour = "black")
  )


boosting_tune_metrics |>
  filter(learn_rate < 0.1 & tree_depth >= 2 & trees <= 5000) |>
  select(trees:learn_rate, .metric, mean) |>
  pivot_wider(
    id_cols = trees:learn_rate,
    names_from = .metric,
    values_from = mean
  ) |>
  ggplot() +
  aes(
    x = specificity, y = sensitivity,
    colour = factor(trees, ordered = TRUE),
    size = learn_rate
  ) +
  geom_point() +
  facet_wrap(~tree_depth, ncol = 1, labeller = label_both) +
  scale_size_continuous(range = c(2, 4), breaks = 10^c(-3, -2)) +
  scale_colour_viridis_d(begin = .3, end = .9, option = "E") +
  theme_bw() +
  labs(colour = "trees")

boosting_tune_metrics |>
  filter(learn_rate == 0.01 & tree_depth == 2 & trees <= 5000) |>
  select(trees:learn_rate, .metric, mean, std_err) |>
  filter(.metric %in% c("sensitivity", "percision")) |>
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
#######

boosting_best <-
  boosting_tune_metrics |>
  filter(tree_depth == 2, learn_rate == 0.1, trees == 3000) |>
  distinct(trees, tree_depth, learn_rate)

boosting_final_wf <-
  finalize_workflow(boosting_tune_wf, boosting_best)
boosting_final_wf

boosting_final_fit <-
  boosting_final_wf |>
  last_fit(analysis_assessment_split, metrics = class_metrics)

boosting_test_results <-
  boosting_final_fit |>
  collect_metrics()
boosting_test_results

predictions <- boosting_final_fit$.predictions[[1]]

confution_matrix<- predictions|>
  conf_mat(truth = Status, estimate = .pred_class)

sensitivity_Boosting <- confution_matrix[1]$table %>% sensitivity()
precision_Boosting <- confution_matrix[1]$table %>% precision()
accuracy_Boosting <- confution_matrix[1]$table %>% accuracy()