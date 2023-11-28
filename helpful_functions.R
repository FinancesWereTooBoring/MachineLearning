# functions that are useful for working with the admissions data

make_appyear_split <- function(d, test_year) {
  rsample::make_splits(
    x = list(analysis = which(d$AppYear < test_year),
             assessment = which(d$AppYear == test_year)),
    data = d)
}


censor_post_prediction_responses <- function(d, 
                                             years = NULL,
                                             prediction_date = "03-15") {
  years <- if (is.null(years)) dplyr::distinct(d, AppYear)$AppYear else years
  
  d |> 
    dplyr::mutate(prediction_date = lubridate::ymd(
      stringr::str_glue(
        "{AppYear}-{prediction_date}")),
      Response = 
        dplyr::case_when(
          ResponseDate < prediction_date - lubridate::days(1) ~ Response,
          !(AppYear %in% years) ~ Response,
          TRUE ~ NA) |>
        forcats::fct_na_value_to_level(level = 'Unknown') |> 
        forcats::fct_relevel('Unknown', after = 1),
      ResponseDate = 
        dplyr::case_when(
          ResponseDate < prediction_date - lubridate::days(1) ~ ResponseDate,
          !(AppYear %in% years)  ~ ResponseDate,
          TRUE ~ NA)) |> 
    dplyr::select(-prediction_date)
}


drop_post_prediction_offers <- function(d, 
                                        years = NULL,
                                        prediction_date = "03-15") {
  years <- if (is.null(years)) dplyr::distinct(d, AppYear)$AppYear else years
  
  d |> 
    dplyr::mutate(prediction_date = lubridate::ymd(
      stringr::str_glue("{AppYear}-{prediction_date}"))) |> 
    dplyr::filter((!(AppYear %in% years)) |
             OfferDate <= prediction_date - lubridate::days(1)) |> 
    dplyr::select(-prediction_date)
}


check_for_missing_factor_levels_in_training_set <- function(split) {
  cols_lvls <- function(x) {
    x |> 
      dplyr::select(where(is.factor), -Status) |> 
      recipes::recipe(~ ., data = _) |> 
      recipes::step_dummy(recipes::all_predictors(), one_hot = TRUE) |> 
      recipes::step_zv(recipes::all_predictors()) |>  
      recipes::prep() |> 
      recipes::bake(new_data = NULL) |> 
      colnames() |> 
      tibble::tibble(cols = _)
  }
  train <- rsample::training(train_prediction_split) |> cols_lvls()
  test <- rsample::testing(train_prediction_split) |> cols_lvls()
  
  if (nrow(test |> dplyr::anti_join(train, by = dplyr::join_by(cols))) == 0) {
    message("Test set does not contain any factor levels that are not present in the training set")
    return(invisible(NULL))
  }
  
  missing <- 
    dplyr::anti_join(test, train, by = dplyr::join_by(cols)) |> 
    getElement('cols') |> 
    stringr::str_c(collapse = ", ")
  
  message(stringr::str_glue("The following factor levels are present in the test set but missing in the training set:\n{missing}."))
}



make_appyear_validation_split <- function(d, validation_year, test_year) {
  res <- list(data = d, 
              train_id = which(d$AppYear < validation_year), 
              val_id = which(d$AppYear == validation_year),
              test_id = which(d$AppYear == test_year), 
              id = "split")
  class(res) <- c("initial_validation_split", "three_way_split")
  res
}
