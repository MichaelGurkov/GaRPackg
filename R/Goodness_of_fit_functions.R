
#' @title Calculate quantile R square score for given spec:
#' realized_values, forecast_values, quantile, benchmark_values
#'
#' @description This function evaluates the goodness of fit between
#' "realized"  value and forecasted quantiles. The calculation is for given spec:
#' realized_values, forecast_values, quantile, benchmark_values
#'
#' @param realized_values realized values series for comparison to forecasted
#'
#' @param forecast_values forecasted values series for comparison to realized
#'
#' @param quantile the quantile corresponding to the realized and forecasted
#' values series
#'
#' @param benchmark_values benchmark_values series (intercept predictions
#'  in GaR model)
#'
quantile_r2_score_calculation = function(realized_values,
                                         forecast_values,
                                         quantile,
                                         benchmark_values) {

  if (length(forecast_values) != length(benchmark_values)) {
    stop("Forecast values and benchmark must be of same length ")

  }


  nom = 0

  denom = 0

  for (ind in 1:length(forecast_values)) {
    realized_error = realized_values[ind] - forecast_values[ind]

    benchmark_error = realized_values[ind] - benchmark_values[ind]

    nom = nom + if_else(realized_error > 0,
                        realized_error * quantile,
                        realized_error * (quantile - 1))

    denom = denom + if_else(benchmark_error > 0,
                            benchmark_error * quantile,
                            benchmark_error * (quantile - 1))


  }

  return(1 - nom / denom)



}


#' @title Calculate quantile R square score
#'
#' @importFrom rlang .data
#'
#' @import dplyr
#'
#' @importFrom zoo as.yearqtr as.yearmon
#'
#' @param forecast_df data frame with forecasted values.
#' by horizon, quantile, forecast_target_date, forecast_values.
#' The forecast_target_date specifies the date for which the forecast is aimed
#'
#' @param actual_df data frame with actual values
#' by date and actual_values
#'
#' @param benchmark_df data frame with forecasted values
#' by horizon, quantile, forecast_target_date, forecast_values.
#' The forecast_target_date specifies the date for which the forecast is aimed
#'
#'
#' @details The evaluation is based on the assumption that the date in
#' \code{forecast_df} refers to the time in which the forecast was performed.
#' Namely, the function offsets each forecast date in \code{forecast_df} by the
#' relevant horizon and matches it with the respective date in \code{actual_df}.
#' For example, a forecast for the horizon of 4 quarters in 1999 Q1 is compared
#' to an actual value in 2000 Q1.
#'
#'
#' @export
#'
quantile_r2_score = function(forecast_df, actual_df, benchmark_df){

  horizon = NULL

  # Arguments Validation

  var_names = c("horizon","quantile","forecast_values", "forecast_target_date")

  if(!all(var_names %in% names(forecast_df))){

    stop("The following variables are missing in predict df :",
         paste(var_names[!var_names %in% names(forecast_df)],
               collapse = ","))


  }

  if(!all(var_names %in% names(benchmark_df))){

    stop("The following variables are missing in benchmark_df :",
         paste(var_names[!var_names %in% names(benchmark_df)],
               collapse = ","))


  }

  if(!"date" %in%  names(actual_df)){

    stop("The date variable is missing in actual df ")


  }

  if(!any(is.character(forecast_df$horizon),is.numeric(forecast_df$horizon))){

    stop("horizon must be of class character or numeric")

  }

  if(!any(is.character(forecast_df$quantile),is.numeric(forecast_df$quantile))){

    stop("quantile must be of class character or numeric")

  }

  forecast_df = forecast_df %>%
    select(all_of(var_names)) %>%
    dplyr::rename(predicted_values = forecast_values)

  benchmark_df = benchmark_df %>%
    dplyr::select(all_of(var_names)) %>%
    dplyr::rename(benchmark_values = forecast_values)

  # names(forecast_df)[!names(forecast_df) %in% var_names] = "predicted_values"
  #
  # names(benchmark_df)[!names(benchmark_df) %in% var_names] = "benchmark_values"

  names(actual_df)[!names(actual_df) == "date"] = "actual_values"


  frequency = identify_frequency(forecast_df$forecast_target_date)


  if(frequency == "quarterly"){

    df = forecast_df %>%
      dplyr::inner_join(benchmark_df,
                 by = c("forecast_target_date", "horizon", "quantile")) %>%
      dplyr::inner_join(actual_df %>%
                   dplyr::mutate(forecast_target_date = as.yearqtr(date)),
                   by = "forecast_target_date") %>%
      dplyr::mutate(quantile = as.numeric(quantile))

  }

  if (frequency == "monthly"){

    df = forecast_df %>%
      dplyr::inner_join(benchmark_df,
                          by = c("forecast_target_date", "horizon", "quantile")) %>%
      dplyr::inner_join(actual_df %>%
                          dplyr::mutate(forecast_target_date = as.yearmon(date)),
                        by = "forecast_target_date") %>%
      dplyr::mutate(quantile = as.numeric(quantile))
  }


  score_df = df %>%
    dplyr::group_by(horizon, quantile) %>%
    dplyr::summarise(
      quantile_r2 =
        quantile_r2_score_calculation(
          realized_values = actual_values,
          forecast_values = predicted_values,
          quantile = quantile[1],
          benchmark_values = benchmark_values
        ),.groups = "drop"
    )

  return(score_df)







}



#' @title Calculate PIT score
#'
#' @details This function calculates the Probability Integral
#' Transformation to evaluate goodness of fit.
#' The value represents the calibration - the relative frequency of the
#' data points in the sample.
#'
#' @importFrom rlang .data
#'
#' @import dplyr
#'
#' @inheritParams quantile_r2_score
#'
#'
#' @details The evaluation is based on the assumption that the date in
#' \code{forecast_df} refers to the time in which the forecast was performed.
#' Namely, the function offsets each forecast date in \code{forecast_df} by the
#' relevant horizon and matches it with the respective date in \code{actual_df}.
#' For example, a forecast for the horizon of 4 quarters in 1999 Q1 is compared
#' to an actual value in 2000 Q1.
#'
#' @export
#'

quantile_pit_score = function(forecast_df, actual_df){

  # Arguments Validation

  var_names = c("horizon","quantile","forecast_values", "forecast_target_date")

  if(!all(var_names %in% names(forecast_df))){

    stop("The following variables are missing in predict df :",
         paste(var_names[!var_names %in% names(forecast_df)],
               collapse = ","))


  }

  if(!"date" %in%  names(actual_df)){

    stop("The date variable is missing in actual df ")


  }

  if(!any(is.character(forecast_df$horizon),is.numeric(forecast_df$horizon))){

    stop("horizon must be of class character or numeric")

  }

  if(!any(is.character(forecast_df$quantile),is.numeric(forecast_df$quantile))){

    stop("quantile must be of class character or numeric")

  }

  names(actual_df)[!names(actual_df) == "date"] = "actual_values"

  forecast_df = forecast_df %>%
    dplyr::select(all_of(var_names)) %>%
    dplyr::rename(predicted_values = forecast_values)


  frequency = identify_frequency(forecast_df$forecast_target_date)


  if(frequency == "quarterly"){

    prediction_df = forecast_df %>%
       dplyr::left_join(actual_df %>%
                  dplyr::mutate(forecast_target_date = as.yearqtr(date)),
                  by = c("forecast_target_date"))

  }

  if (frequency == "monthly"){

    prediction_df = forecast_df %>%
      dplyr::left_join(actual_df %>%
                  dplyr::mutate(forecast_target_date = as.yearmon(date)),
                  by = c("forecast_target_date"))
  }


  if(sum(is.na(prediction_df$actual_values)) > 0){

    warning(paste0("There are missing actual values",
                   " for corresponding predicted values.",
                   "\n",
                   "Those values will be excluded"))

  }


  pit_score_df = prediction_df %>%
    dplyr::filter(!is.na(actual_values)) %>%
    dplyr::group_by(horizon, quantile) %>%
    dplyr::mutate(pit = if_else(actual_values < predicted_values,
                         1 /length(date),0)) %>%
    dplyr::summarise(pit = sum(pit), .groups = "drop")

  return(pit_score_df)


}


#' @title Calculate prediction score
#'
#' @details This function calculates the prediction score which is the
#' probability to get the observed (actual) value from the fitted t skew
#' distribution based on estimated quantiles
#'
#' @importFrom rlang .data
#'
#' @import dplyr
#'
#' @importFrom sn dst
#'
#' @param forecast_dist_df data frame with estimated t skewed distribution
#' by horizon, quantile, date and parameter
#'
#' @param actual_df data frame with actual values
#' by value and date
#'
#' @export
#'

quantile_prediction_score = function(forecast_dist_df, actual_df){

  # Arguments validation

  var_names = c("parameter", "value")

  if(!all(var_names %in% names(forecast_dist_df))){

    stop("The following variables are missing in forecast dist df :",
         paste(var_names[!var_names %in% names(forecast_dist_df)],
               collapse = ","))


  }

  if(!"date" %in%  names(actual_df)){

    stop("The date variable is missing in actual df ")


  }

  names(actual_df)[!names(actual_df) == "date"] = "actual_value"

  prediction_score = dplyr::left_join(forecast_dist_df, actual_df,
                               by = "date") %>%
    dplyr::group_by(across(-c("parameter", "value"))) %>%
    dplyr::summarise(prob = dst(x = actual_value[1], dp = value),
              .groups = "drop")

  return(prediction_score)




}
