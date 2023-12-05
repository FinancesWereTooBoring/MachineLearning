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

####boosting##

#tuning choose number of folds
set.seed(82001)
cv_folds <- train |> vfold_cv(v = 10)
