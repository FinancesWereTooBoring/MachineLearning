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

#########

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
