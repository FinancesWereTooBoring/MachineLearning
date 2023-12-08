#-------------------
# knn
#-------------------

source('./data_processing.R')
source('./data_split_ct.R')
load('./offers_censored.RData')
head(offers)
str(offers)

library(class)
library(caret)

#checking for imbalance 
offers |>
  count(Status) |>
  mutate(prop = n / sum(n))

#------------------------------------------------------------------
#0 check balance after splitting

analysis_train |>
  count(Status) |>
  mutate(prop = n / sum(n))

#0. explore 
analysis_train |>
  count(Status, Response) |>
  group_by(Status) |>
  mutate(prop = n / sum(n))

#1. specify model
knn_model <-
  nearest_neighbor(neighbors = tune()) |>
  set_mode("classification") |>
  set_engine("kknn")

#2. recipe: normalize numeric to have mean = 0 
knn_recipe <-
  recipe(Status ~ ., 
         data = analysis_train) |>
  step_normalize(App4) |> #only integer value needed for step 5
  update_role(AppDate, OfferDate, ResponseDate, new_role = "id var") |>
  step_dummy(all_nominal_predictors()) |> 
  #removes predictors w 0 variance
  step_zv(all_predictors()) 
knn_recipe

#3. create workflow object - combine model + recipe
knn_workflow <-
  workflow() |>
  add_model(knn_model) |>
  add_recipe(knn_recipe)
knn_workflow

# Set up a cross-validation control
set.seed(666420)
cv_folds <- vfold_cv(analysis_train, v = 10, strata = "Status")

#4, set tuning grid 
#two options
#need larger numbers for k since n is larger here
knn_tune_grid <- grid_regular(neighbors(range = c(1, 17)), #check if range works
                              levels = 17)
knn_tune_grid

#im not super sure which one
knn_class_tune_grid <- tibble(neighbors = 5:20 * 2 + 1)
knn_class_tune_grid

#5. Tuning n nearest neighbors
  #analysis <- validation_set(analysis_assessment_split)
knn_tune_results <-
  knn_workflow |>
  tune_grid(
    resamples = cv_folds,
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
  show_best("accuracy", n = 5) |>
  arrange(desc(mean), desc(neighbors))
#13 best neighbors 

#9. select best k neighbors - highest value for accuracy
knn_best_model <-
  knn_tune_results |>
  select_best(metric = "accuracy")
knn_best_model

#10. finalize workflow
knn_workflow_final <-
  knn_workflow |>
  finalize_workflow(knn_best_model)
knn_workflow_final

# 1SE rule 
knn_1se_model <- 
  knn_tune_results |> 
  select_by_one_std_err(metric = "accuracy", desc(neighbors))

knn_workflow_tuned <- 
  knn_workflow |> 
  finalize_workflow(knn_1se_model)

#11. test on test set 

knn_class_last_fit <-
  knn_workflow_final |>
  last_fit(analysis_assessment_split,
           metrics = metric_set(
             kap, f_meas,
             bal_accuracy, accuracy
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

#              Truth
#Prediction     Enrolled Not enrolled
#Enrolled          947 TP         416 FP
#Not enrolled       70          203

sensitivity_knn <- conf_mat_knn[1]$table %>% sensitivity()
# 0.9311701
precision_knn <- conf_mat_knn[1]$table %>% precision()
# 0.6947909
accuracy_knn <- conf_mat_knn[1]$table %>% accuracy()
# 0.703

lasso_final_model <- lasso_wf_tuned %>% fit(data =final_training)
