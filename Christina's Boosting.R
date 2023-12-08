set.seed(82001)
cv_folds <- analysis_train |> vfold_cv(v = 5, strata = Status)#change later to 10


boosting_recipe <-
  recipe(Status ~ ., data = offers) |>
  # remove these variables:
  step_rm(AppDate, OfferDate, ResponseDate) |>
  # convert to dummy variables:
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |>
  themis::step_downsample(Status)

boosting_model_tune <-
  boost_tree(
    trees = tune(),
    tree_depth = tune(),
    learn_rate = tune(),
    stop_iter = 500
  ) |>
  set_mode("classification") |>
  set_engine("xgboost")

boosting_tune_wf <-
  workflow() |>
  add_recipe(boosting_recipe) |>
  add_model(boosting_model_tune)
boosting_tune_wf

class_metrics <- metric_set(
  accuracy, kap, sensitivity,
  specificity, roc_auc
)

num_cores <- parallel::detectCores()
num_cores

doParallel::registerDoParallel(cores = num_cores - 1L)

set.seed(8504)
grid_max_entropy(trees(range = c(0, 10000)),
                 learn_rate(range = c(-2, -1)),
                 tree_depth(),
                 size = 20)

boosting_grid <- crossing(
  trees = 500 * 1:10,#change later to 20
  learn_rate = c(0.1, 0.01, 0.001),
  tree_depth = c(1, 2, 3))
boosting_grid

boosting_tune_res <- tune_grid(
  boosting_tune_wf,
  resamples = cv_folds,
  grid = boosting_grid,
  metrics = class_metrics
)
#####

boosting_tune_metrics <-
  boosting_tune_res |>
  collect_metrics()
boosting_tune_metrics

boosting_tune_metrics |>
  filter(.metric == "accuracy") |>
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
  ggplot(aes(x = trees, y = mean, colour = factor(tree_depth))) +
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

boosting_tune_metrics |>
  filter(.metric %in% c("sensitivity", "specificity")) |>
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


boosting_best <-
  boosting_tune_metrics |>
  filter(tree_depth == 3, learn_rate == 0.1, trees == 3500) |>
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

# A tibble: 5 × 4
#.metric     .estimator .estimate .config             
#<chr>       <chr>          <dbl> <chr>               
# 1 accuracy    binary         0.933 Preprocessor1_Model1
#2 kap         binary         0.861 Preprocessor1_Model1
#3 sensitivity binary         0.899 Preprocessor1_Model1
#4 specificity binary         0.989 Preprocessor1_Model1
#5 roc_auc     binary         0.966 Preprocessor1_Model1
