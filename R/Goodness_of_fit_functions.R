
#' @title Calculate quantile R square score
#'
#' @description This function evaluates the goodness of fit between
#' "realized"  value and forecasted quantiles
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
quantile_r2_score = function(realized_values, forecast_values,
                             quantile, benchmark_values){

  if(length(forecast_values) != length(benchmark_values)){

    stop("Forecast values and benchmark must be of same length ")

  }


  nom = 0

  denom = 0

  for (ind in 1:length(forecast_values)){

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
#' @param forecast_df data frame with predicted values
#' by horizon, quantile and date
#'
#' @param actual_df data frame with actual values
#' by value and date
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
