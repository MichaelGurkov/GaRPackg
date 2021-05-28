
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
#' @param predict_df data frame with predicted values
#' by horizon, quantile, date, forecast_values
#'
#' @param actual_df data frame with actual values
#' by date and actual_values
#'
#' @param benchmark_df data frame with predicted values
#' by horizon, quantile, date and benchmark_values
#'
#'
#' @details The evaluation between predicted and actual (benchmark) values is
#' done by comparing the values for a given date. That is based on the
#' assumption that the dates are correctly aligned. For instance, if the metric
#' is Year on Year growth that means that on a given day we have the change
#' for the previous year. So on 2000 Q1 we know the realized change for
#' the period 1999 Q1-2000 Q1. Since the forecast is given for various horizons
#' that means that a given date is the combination of the actual forecast date
#' and the forecast horizon. So a forecast for an horizon of 4 quarters on
#' 1999 Q1 and a forecast for an horizon of 8 quarters on 1998 Q3 both predict
#' the realized change for the period 1999 Q1-2000 Q1.

#'
#' @export
#'
quantile_r2_score = function(predict_df, actual_df, benchmark_df){

  var_names = c("horizon","quantile","date")

  if(!all(var_names %in% names(predict_df))){

    stop("The following variables are missing in predict df :",
         paste(var_names[!var_names %in% names(predict_df)],
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

  names(predict_df)[!names(predict_df) %in% var_names] = "predicted_values"

  names(benchmark_df)[!names(benchmark_df) %in% var_names] = "benchmark_values"

  names(actual_df)[!names(actual_df) == "date"] = "actual_values"


  df = predict_df %>%
    inner_join(benchmark_df,
              by = c("date", "horizon", "quantile")) %>%
    inner_join(actual_df, by = "date") %>%
    mutate(quantile = as.numeric(.data$quantile))

  score_df = df %>%
    group_by(.data$horizon, .data$quantile) %>%
    summarise(
      quantile_r2 =
        quantile_r2_score_calculation(
          realized_values = .data$actual_values,
          forecast_values = .data$predicted_values,
          quantile = .data$quantile[1],
          benchmark_values = .data$benchmark_values
        ),.groups = "drop"
    )

  return(score_df)







}



#' @title Calculate PIT score
#'
#'
#' @details This function calculates the Probability Integral
#' Transformation to evaluate goodness of fit.
#' The value represents the relative frequency of the
#' data points in the sample
#'
#' @importFrom rlang .data
#'
#' @import dplyr
#'
#' @param forecast_df data frame with predicted values
#' by horizon, quantile and date
#'
#' @param actual_df data frame with actual values
#' by value and date
#'
#' @export
#'

quantile_pit_score = function(forecast_df, actual_df){

  var_names = c("horizon","quantile","date")

  if(!all(var_names %in% names(forecast_df))){

    stop("The following variables are missing in forecast df :",
         paste(var_names[!var_names %in% names(forecast_df)],
               collapse = ","))


  }

  if(!"date" %in%  names(actual_df)){

    stop("The date variable is missing in actual df ")


  }

  names(forecast_df)[!names(forecast_df) %in% var_names] = "predicted_values"

  names(actual_df)[!names(actual_df) == "date"] = "actual_values"


  prediction_df = forecast_df %>%
    mutate(date = as.yearqtr(.data$date) + as.numeric(.data$horizon) * 0.25) %>%
    left_join(actual_df %>%
                mutate(date = as.yearqtr(.data$date)), by = c("date"))


  if(sum(is.na(prediction_df$actual_values)) > 0){

    warning(paste0("There are missing actual values",
                   " for corresponding predicted values.",
                   "\n",
                   "Those values will be excluded"))

  }


  pit_score_df = prediction_df %>%
    filter(!is.na(.data$actual_values)) %>%
    group_by(.data$horizon, .data$quantile) %>%
    mutate(pit = if_else(.data$actual_values < .data$predicted_values,
                         1 /length(.data$date),0)) %>%
    summarise(pit = sum(.data$pit), .groups = "drop")

  return(pit_score_df)


}


#' @title Calculate prediction score
#'
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

  prediction_score = left_join(forecast_dist_df, actual_df,
                               by = "date") %>%
    group_by(across(-c("parameter", "value"))) %>%
    summarise(prob = dst(x = .data$actual_value[1], dp = .data$value),
              .groups = "drop")

  return(prediction_score)




}
