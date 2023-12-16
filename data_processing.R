library(tidymodels)
library(tidyverse)

load("data/offers_uncensored.RData")
source("./helpful_functions.R")

<<<<<<< HEAD
set.seed(666420)

#prediction
=======
# We need to set a seed
set.seed(666420)

>>>>>>> main
final_training_prediction_split <-
  offers |>
  make_appyear_split(test_year = 2023)

#final_training_prediction_split <- initial_split(data = final_training_prediction_split, strata = Status)

final_training <- training(final_training_prediction_split)

#training
analysis_assessment_split <-
  offers |>
  censor_post_prediction_responses(years = 2022)|>
  drop_post_prediction_offers()|>
  filter(AppYear <= 2022) |>
  make_appyear_split(test_year = 2022)

analysis_train <- training(analysis_assessment_split)
<<<<<<< HEAD
assessment_test <- testing(analysis_assessment_split)
=======
assessment_test <- testing(analysis_assessment_split)
>>>>>>> main
