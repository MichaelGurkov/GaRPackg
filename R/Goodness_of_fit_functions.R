#' @title Calculate Osnat's quantile fit score
#'
#' @description This function evaluates the goodness of fit between
#' "realized" and to forecasted CDF
#'
#' @details
#'
quantile.fit.score = function(realized_value, quantile_values, quantiles){

  if(length(quantile_values) != length(quantiles)){

    stop("Quantile values and quantiles must be of same length ")

  }

  score = 0

  for (ind in seq_along(quantiles)){

    if(quantile_values[ind] < realized_value){

      score = score + quantiles[ind] ^ 2

    } else {

      score = score + (1 - quantiles[ind]) ^ 2

    }

  }

  return(score)

}


#' @title Calculate Osnat's augmented quantile fit score
#'
#' @description This function evaluates the goodness of fit between
#' "realized" and to forecasted CDF by "area" method
#'
quantile.fit.score.area = function(realized_value,
                                   quantile_values,
                                   quantiles,
                                   min_quantile_value = -0.2,
                                   max_quantile_value = 0.2){

  # Data validation

  if(length(quantile_values) != length(quantiles)){

    stop("Quantile values and quantiles must be of same length ")

  }

  score = 0

  # Construct named vector to hold values and quantiles
  # the names of the vector are quantiles and the values are
  # quantile values

  # Construct vector to hold values and quantiles

  vec = c(min_quantile_value,quantile_values, max_quantile_value)

  names(vec) = c(0,quantiles,1)

  vec = c(vec, realized_value)

  vec = vec[order(vec)] # order vec (quantile values + realization)

  real_est_ind = which(vec == realized_value)

  # Calculate realized estimate prob (the height)
  # the weight is calculated as w = (B - C) / (A-C)
  # if B = w * A + (1-w) * C

  weight = (vec[real_est_ind] - vec[real_est_ind + 1]) /
    (vec[real_est_ind - 1] - vec[real_est_ind + 1])

  real_est_prob = weight * as.numeric(names(vec))[real_est_ind - 1] +
    (1 - weight) * as.numeric(names(vec))[real_est_ind + 1]


  names(vec)[real_est_ind] = real_est_prob

  # Calculate score:

  # Summarize quantile below realized estimates

  for (ind in 2:real_est_ind){

    score = score +
      0.5 * sum((as.numeric(names(vec)[c(ind - 1,ind)]))) *
      diff(vec[c(ind - 1,ind)])



  }

  # Summarize quantile above realized estimates

  for (ind in (real_est_ind + 1): length(vec)){

    score = score +
      0.5 * sum((1 - as.numeric(names(vec)[c(ind - 1,ind)]))) *
      diff(vec[c(ind - 1,ind)])

  }

  names(score) = NULL

  return(score)

}



#' @title Calculate continuous ranked probability score
#'
#' @description This function evaluates the goodness of fit between
#' "realized" and to forecasted CDF by "area" method
#'
quantile.crps.score = function(realized_value,
                               quantile_values,
                               quantiles,
                               min_quantile_value = -0.2,
                               max_quantile_value = 0.2){

  # Data validation

  if(length(quantile_values) != length(quantiles)){

    stop("Quantile values and quantiles must be of same length ")

  }

  if(realized_value < min_quantile_value){

    message("Realized estimate is less than min quantile value")

    return(NA)


  }

  if(realized_value > max_quantile_value){

    message("Realized estimate is greater than max quantile value")

    return(NA)


  }

  # Construct named vector to hold values and quantiles
  # the names of the vector are quantiles and the values are
  # quantile values

  # Construct vector to hold values and quantiles

  vec = c(min_quantile_value,quantile_values, max_quantile_value)

  names(vec) = c(0,quantiles,1)

  vec = c(vec, realized_value)

  vec = vec[order(vec)] # order vec (quantile values + realization)

  real_est_ind = which(vec == realized_value)

  # Calculate realized estimate prob (the height)
  # the weight is calculated as w = (B - C) / (A-C)
  # if B = w * A + (1-w) * C

  weight = (vec[real_est_ind] - vec[real_est_ind + 1]) /
    (vec[real_est_ind - 1] - vec[real_est_ind + 1])

  real_est_prob = weight * as.numeric(names(vec))[real_est_ind - 1] +
    (1 - weight) * as.numeric(names(vec))[real_est_ind + 1]


  names(vec)[real_est_ind] = real_est_prob

  # Calculate score:

  quantile_values = as.numeric(vec)

  quantiles = as.numeric(names(vec))

  score = 0

  # Summarize quantile below realized estimates

  for (ind in 2:real_est_ind){

    score = score +
      diff(quantile_values[c(ind - 1,ind)]) *
      mean(c(quantiles[ind] ^ 2,quantiles[ind] * quantiles[ind - 1],
             quantiles[ind- 1]  ^ 2))



  }

  # Summarize quantile above realized estimates

  for (ind in (real_est_ind + 1): length(vec)){

    score = score +
      diff(quantile_values[c(ind - 1,ind)]) *
      mean(c((1 -quantiles[ind]) ^ 2,
             (1 - quantiles[ind]) * (1 - quantiles[ind - 1]),
             (1 - quantiles[ind- 1])  ^ 2))

  }

  return(score)

}




#' @title Calculate quantile R square score
#'
#' @description This function evaluates the goodness of fit between
#' "realized"  value and forecasted quantiles
#'
quantile.r2.score = function(realized_values, forecast_values,
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
#' @details This function calculates the Probability Integral
#' Transformation to evaluate goodness of fit.
#' The value represents the relative frequency of the
#' data points in the sample
#'
#' @param prediction_df data frame with actual values and
#' predicted values by horizon, quantile and date
#'

quantile.pit.score = function(prediction_df){

  T_sample = length(unique(prediction_df$Date))

  pit_score_df = prediction_df %>%
    mutate(pit = if_else(actual_values < predicted_values,
                         1 /T_sample,0)) %>%
    group_by(Horizon,Quantile) %>%
    summarise(pit = sum(pit), .groups = "drop")

  return(pit_score_df)


}
