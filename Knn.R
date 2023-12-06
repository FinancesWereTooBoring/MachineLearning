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



#-------------------------------------
# just in case 
# splitting
#-------------------------------------

library(ggridges)
library(dplyr)
library(ggplot2)
load("offers_censored.RData")
new_data <- offers |> filter(AppYear != 2023)

table(new_data$Status)
table(new_data$Response)
status_response_table <- table(new_data$Status, new_data$Response)
print(status_response_table)

new_data$BeforeMarch15 <- ifelse(format(new_data$ResponseDate, "%m-%d") < "03-15", "Before", "After")
new_data$BeforeMarch15[is.na(new_data$BeforeMarch15)] <- "NA_Values"
before_after_na_counts <- table(new_data$BeforeMarch15)
print(before_after_na_counts)

na_values_count_nonunknown <- sum(new_data$BeforeMarch15 == "NA_Values" & new_data$Response != "Unknown")
print(na_values_count_nonunknown)
selected_rows <- new_data[new_data$BeforeMarch15 == "NA_Values" & new_data$Response != "Unknown", ]
print(selected_rows)

#drop them#
new_data <- new_data[!(new_data$BeforeMarch15 == "NA_Values" & new_data$Response != "Unknown"), ]


contingency_tables <- lapply(unique(new_data$AppYear), function(year) {
  year_data <- new_data[new_data$AppYear == year, c( "Status","Response", "BeforeMarch15")]
  
  table_by_year <- table(year_data$Status,year_data$Response,  year_data$BeforeMarch15)
  
  print(paste("Contingency Table for Year", year))
  print(table_by_year)
  
  return(table_by_year)
})

new_data |> 
  ggplot(aes(x = Response, fill = Status)) +
  geom_bar(position = "fill") +
  scale_fill_viridis_d(option = 'E', direction = -1, end = .8) +
  theme_bw() 

new_data |>
  ggplot(aes(x = Response, fill = Status)) +
  geom_bar(position = "fill") +
  scale_fill_viridis_d(option = 'E', direction = -1, end = .8) +
  theme_bw() +
  facet_wrap(~AppYear)


contingency_table <- table(new_data$BeforeMarch15, new_data$Demo1)

# Perform chi-squared test
chi_squared_test <- chisq.test(contingency_table)

##############################

library(tidymodels)
library(tidyverse)
library(survey)
library(xgboost)

source("helpful_functions.R")
years_and_max_dates <- function(new_data) {
  new_data |>
    group_by(AppYear) |>
    summarise(
      `Num observations` = n(),
      `Max \`AppDate\`` = max(AppDate),
      `Max \`OfferDate\`` = max(OfferDate),
      `Max \`ResponseDate\`` = max(ResponseDate, na.rm = TRUE),
      `\`ResponseDate\` is NA` = sum(is.na(ResponseDate))
    )
}

final_training_prediction_split <-
  new_data |>
  # change responses received after March 14 to "Unknown"
  censor_post_prediction_responses() |>
  # drop offers sent out after March 14
  drop_post_prediction_offers() |>
  # split 2023 into the prediction set, and retain the rest
  # as the final training set
  make_appyear_split(test_year = 2023)


training(final_training_prediction_split) |> years_and_max_dates()

analysis_assessment_split <-
  new_data |>
  filter(AppYear <= 2022) |>
  # change responses after March 14 to "Unknown", but only in 2022
  censor_post_prediction_responses(years = 2022) |>
  # drop offers sent out after March 14, but only in 2022
  drop_post_prediction_offers(years = 2022) |>
  make_appyear_split(test_year = 2022)


train<-training(analysis_assessment_split) |> years_and_max_dates()

test<-testing(analysis_assessment_split) |> years_and_max_dates()

#------------------------------------------------------------------

#1. specify model
knn_model <-
  nearest_neighbor(neighbors = tune()) |>
  set_mode("classification") |>
  set_engine("kknn")

#2. recipe: normalize numeric to have mean = 0 
knn_recipe <-
  recipe(Status ~ AppYear + Program + Response 
         + Demo1 + Demo2 + Demo3 
         + App1 + App2 + App3 + App4 
         + HowFirstHeard, #predictors & control vars
         data = analysis_train
  ) |>
  step_normalize()

#3. create workflow object - combine model + recipe
knn_workflow <-
  workflow() |>
  add_model(knn_model) |>
  add_recipe(knn_recipe)
knn_workflow

# Set up a cross-validation control
cv_folds <- vfold_cv(analysis_train, v = 10)

#4, set tuning grid 
#need larger numbers for k since n is larger here
knn_tune_grid <- grid_regular(neighbors(range = c(5, 60)),
                              levels = 20)
knn_tune_grid

#5. Tuning n nearest neighbors
  #analysis <- validation_set(analysis_assessment_split)
knn_tune_results <-
  knn_workflow |>
  tune_grid(
    resamples = cv_folds,
    grid = knn_tune_grid,
    metrics = metric_set(
      kap , f_meas, #all mnetrics as close to one as possible for good measure
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
