
#' @title Calculate t skewed distribution fitting loss
#'
#' @description This function calculates the loss of matching estimated quantiles
#' with theoretical ones. The errors are calculated as the difference between
#' empirical and theoretical values for each quantile. The loss metric is RMSE.
#'
#' @importFrom sn qst
#'
#' @importFrom magrittr "%>%"
#'
#' @importFrom rlang .data
#'
#' @importFrom stats optim
#'
#' @param estimated_df data frame with (quantiles, values) columns
#'
#' @param t_skew_params the parameters of the t skewed distribution
#' xi (location), omega (scale), alpha (slant) and nu (degrees of freedom)

t_skew_loss = function(estimated_df, t_skew_params) {

  quantiles_vec = unique(estimated_df$quantile)

  skew_t_values = qst(
    p = quantiles_vec,
    xi = t_skew_params[1],
    omega = t_skew_params[2],
    alpha = t_skew_params[3],
    nu = t_skew_params[4]
  )

  skew_t_df = tibble::tibble(quantile = quantiles_vec,
                     skew_t_values = skew_t_values)

  loss = estimated_df %>%
    dplyr::left_join(skew_t_df, by = "quantile") %>%
    dplyr::mutate(error = values - skew_t_values) %>%
    dplyr::summarise(loss = sqrt(mean(error ^ 2))) %>%
    dplyr::pull(loss)


  return(loss)

}

#' @title Run t skewed optimization
#'
#' @importFrom rlang .data
#'
#' @importFrom stats sd
#'
#' @description This function finds the parameters of the t skewed distribution
#' xi (location), omega (scale), alpha (slant) and nu (degrees of freedom)
#' while minimizing the RMSE loss function. The function supports bounded and
#' unbounded optimization, the default is bounded optimization.
#'
#' @param estimated_df_x data frame with (quantiles, values) columns
#'
#' @param bounded_optimization_x boolean indicator, default TRUE
#'
#' @param lower_bounds_x lower bounds for xi (location),
#' omega (scale), alpha (slant), nu (degrees of freedom).
#' The default is (-Inf, 0, -Inf, 1).
#'
#' @param upper_bounds_x upper bounds for xi (location),
#' omega (scale), alpha (slant), nu (degrees of freedom).
#' The default is (Inf, Inf, Inf, 100).
#'
run_t_skew_optimization = function(estimated_df_x,
                              bounded_optimization_x = TRUE,
                              lower_bounds_x = c(-Inf, 0.01, -Inf, 1),
                              upper_bounds_x = c(Inf, Inf, Inf, 100)){

  if(!length(setdiff(names(estimated_df_x),c("values","quantile"))) == 0){

    stop("estimated df should only have two columns : quantile and values")
  }

  estimated_mean = estimated_df_x %>%
    dplyr::summarise(est_mean = mean(values, na.rm = TRUE)) %>%
    dplyr::pull(est_mean)

  estimated_sd = estimated_df_x %>%
    dplyr::summarise(est_sd = sd(values, na.rm = TRUE)) %>%
    dplyr::pull(est_sd)


  initial_params = c(
    xi = estimated_mean,
    omega = estimated_sd,
    alpha = 0,
    nu = 30
  )

  if(bounded_optimization_x){

    optimization_result = optim(
      par = initial_params,
      fn = t_skew_loss,
      lower = lower_bounds_x,
      upper = upper_bounds_x,
      method = "L-BFGS-B",
      estimated_df = estimated_df_x
    )


  } else{

    optimization_result = optim(par = initial_params,
                                fn = t_skew_loss,
                                estimated_df = estimated_df_x,
                                method = "Nelder-Mead")


  }




  return(optimization_result$par)

}


#' @title Fit t skewed distribution
#'
#' @description This function fits t skewed distribution based on empirical
#' quantiles. The input data should be a data frame with (quantiles, values)
#' columns.
#'
#' @details The fitting is performed by calculating the errors
#' (difference between empirical and theoretical values for each quantile).
#' The loss metric is RMSE.
#'
#' @param estimated_df data frame with (quantiles, values) columns
#'
#' @param time_limit the time limit given to each optimization round.
#'  In case the computation doesn't converge in the given time frame a vector
#'  of 0 is returned for the optimization parameters and "timed out" warning is
#'  issued.
#'
#' @param bounded_optimization boolean indicator, default TRUE
#'
#' @param lower_bounds lower bounds for xi (location),
#' omega (scale), alpha (slant), nu (degrees of freedom).
#' The default is (-Inf, 0.01, -Inf, 0).
#'
#' @param upper_bounds upper bounds for xi (location),
#' omega (scale), alpha (slant), nu (degrees of freedom).
#' The default is (Inf, Inf, Inf, 100).
#'
#' @return vector of parameters xi (location),
#' omega (scale), alpha (slant), nu (degrees of freedom) if the fitting was
#'  successful, otherwise a vector of 0 if the optimization has timeout or a
#'  vector of NA if there was an error.
#'
#'
#' @export
#'
fit_t_skew = function(estimated_df,time_limit = 10,
                                   bounded_optimization = TRUE,
                                   lower_bounds = c(-Inf, 0.01, -Inf, 1),
                                   upper_bounds = c(Inf, Inf, Inf, 100)){

  # Validation

  if(!length(setdiff(names(estimated_df),c("values","quantile"))) == 0){

    stop("estimated df should only have two columns : quantile and values")
  }

  col_class = purrr::map_chr(estimated_df, class)

  non_numeric_vars = names(col_class)[!col_class == "numeric"]

  if(length(non_numeric_vars) > 0){

    warning(paste("Column(s)",paste(non_numeric_vars, collapse = ","),
                  "have been converted to numeric"))

    estimated_df = estimated_df %>%
      dplyr::mutate(across(dplyr::all_of(non_numeric_vars), as.numeric))


  }

  setTimeLimit(cpu = time_limit, elapsed = time_limit, transient = TRUE)
  on.exit({
    setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
  })

  tryCatch({
    run_t_skew_optimization(estimated_df_x = estimated_df,
                       bounded_optimization_x = bounded_optimization,
                       lower_bounds_x = lower_bounds,
                       upper_bounds_x = upper_bounds)
  }, error = function(e) {
    if (grepl("reached elapsed time limit|reached CPU time limit", e$message)) {
      warning("t skew fitting has timed out")
      vec = c(0,0,0,0)
      names(vec) = c("xi","omega","alpha","nu")
      return(vec)
    } else {
      # error not related to timeout
      vec = c(NA,NA,NA,NA)
      names(vec) = c("xi","omega","alpha","nu")
      return(vec)
    }
  })


}

#' @title Fit t skewed distribution to gar df
#'
#' @description This function fits t skewed distribution to either fitted
#'  values of gar model or out of sample forecasted values
#'
#' @importFrom furrr future_map
#'
#' @importFrom rlang .data
#'
#' @param gar_df data frame. The result of run_GaR_analysis
#'
#' @param return_smoothed_quantiles boolean returns quantiles
#'
#' @param smoothed_quantiles_vec vector of required quantiles.
#' Default is c(0.05, 0.25, 0.5, 0.75, 0.95)
#'
#' @param time_limit the time limit given to each optimization round.
#'  In case the computation doesn't converge in the given time frame a vector
#'  of 0 is returned for the optimization parameters and "timed out" warning is
#'  issued.
#'
#' @param bounded_optimization boolean indicator, default TRUE
#'
#' @param lower_bounds lower bounds for xi (location),
#' omega (scale), alpha (slant), nu (degrees of freedom).
#' The default is (-Inf, 0, -Inf, 0).
#'
#' @param upper_bounds upper bounds for xi (location),
#' omega (scale), alpha (slant), nu (degrees of freedom).
#' The default is (Inf, Inf, Inf, 100).
#'
#' @param parallel_computing boolean. If TRUE uses parallel computing
#' by setting \code{plan(multisession)} before fitting t skewed distribution
#' and setting \code{plan(sequential)} after the fitting is complete.
#'
#' @export
#'
fit_t_skew_to_gar_df = function(gar_df,
                                return_smoothed_quantiles = FALSE,
                                smoothed_quantiles_vec = c(0.05, 0.25, 0.5, 0.75, 0.95),
                                time_limit = 10,
                                bounded_optimization = TRUE,
                                lower_bounds = c(-Inf, 0.01,-Inf, 1),
                                upper_bounds = c(Inf, Inf, Inf, 100),
                                parallel_computing = FALSE)
{
  required_cols = c("quantile", "horizon", "date")

  if (length(setdiff(required_cols, names(gar_df))) > 0) {
    stop(paste0(
      "The following column(s) are missing:",
      paste0(setdiff(required_cols, names(gar_df)),
             collapse = ",")
    ))

  }

  if ("forecast_values" %in% names(gar_df)) {
    gar_df = gar_df %>%
      dplyr::rename(values = forecast_values)

  }

  else if ("fitted_values" %in% names(gar_df)) {
    gar_df = gar_df %>%
      dplyr::rename(values = fitted_values)

  } else {
    stop(
      paste0(
        "Couldn't identify values column, ",
        "must be either forecast_values or fitted_values"
      )
    )


  }

  if(parallel_computing){

    future::plan("multisession")

  }


  nested_df = gar_df %>%
    dplyr::mutate(across(c(quantile, horizon), as.numeric)) %>%
    dplyr::group_by(date, horizon) %>%
    tidyr::nest(data = c(quantile, values)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(t_skew = future_map(data, function(temp_data) {
      temp_fit = fit_t_skew(temp_data,
                            bounded_optimization = bounded_optimization,
                            time_limit = time_limit,
                            lower_bounds = lower_bounds,
                            upper_bounds = upper_bounds)

      fit_df = tibble::tibble(t_skew_parameter = names(temp_fit),
                      values = temp_fit)

      return(fit_df)


    }))

  if(parallel_computing){

    future::plan("sequential")

  }

  t_skew_fit_df = nested_df %>%
    dplyr::select(-data) %>%
    tidyr::unnest(cols = "t_skew")

  if(return_smoothed_quantiles){

    smoothed_quantiles_df = extract_smoothed_quantiles(dist_param_df = t_skew_fit_df,
                                                       raw_quantiles_df = gar_df)
    return(smoothed_quantiles_df)

  }


  return(t_skew_fit_df)





}

#' This is an auxiliary function that returns quantiles from specified
#' t skew distribution
#'
#' @importFrom sn qst
#'
#' @importFrom rlang .data
#'
#' @param t_skew_param_df data frame that specifies distribution parameters.
#' The structure is (date, horizon, t_skew_params, values)
#'
#' @param quantiles_vec vector of required quantiles.
#' Default is c(0.05,0.25,0.5,0.75,0.95)
#'
#'
#'
get_t_skew_quantiles = function(t_skew_param_df,
                                quantiles_vec = c(0.05,0.25,0.5,0.75,0.95)){

  temp_dp = t_skew_param_df %>%
    dplyr::mutate(t_skew_parameter = factor(t_skew_parameter,
                                     levels = c("xi", "omega", "alpha", "nu"))) %>%
    arrange(t_skew_parameter) %>%
    dplyr::pull(values)

  if(all(is.na(temp_dp)) | all(temp_dp == 0)){

    temp_quantiles_df = data.frame(quantiles = quantiles_vec,
                                   values = rep(NA,length(quantiles_vec)))
  } else {

    temp_quantiles_df = data.frame(quantiles = quantiles_vec,
                                   values = qst(p = quantiles_vec, dp = temp_dp))

  }

  return(temp_quantiles_df)



}



#' This function returns "smoothed" (drawn from parametrized t skew distribution)
#' quantiles
#'
#' @importFrom rlang .data
#'
#' @param dist_param_df data frame that specifies distribution parameters.
#' The structure is (date, horizon, t_skew_params, values)
#'
#' @param raw_quantiles_df data frame that specifies t skew fitted quantiles
#' The structure is (date, horizon, quantile, values)
#'
#' @param smoothed_quantiles_vec_x vector of required quantiles.
#' Default is c(0.05, 0.25, 0.5, 0.75, 0.95)
#'
#'
extract_smoothed_quantiles = function(
  dist_param_df,
  raw_quantiles_df,
  smoothed_quantiles_vec_x = c(0.05, 0.25, 0.5, 0.75, 0.95)) {

  smoothed_quantiles_df = dist_param_df %>%
    dplyr::group_by(date, horizon) %>%
    tidyr::nest(cols = c("t_skew_parameter", "values")) %>%
    dplyr::mutate(
      t_skew_quantiles = purrr::map(cols, get_t_skew_quantiles,
                             quantiles_vec = smoothed_quantiles_vec_x)
    ) %>%
    dplyr::select(-cols) %>%
    tidyr::unnest(cols = c("t_skew_quantiles")) %>%
    dplyr::ungroup() %>%
    dplyr::rename(quantile = quantiles) %>%
    dplyr::mutate(horizon = as.character(horizon))


  smoothed_quantiles_df = raw_quantiles_df %>%
    dplyr::mutate(quantile = as.numeric(quantile)) %>%
    dplyr::left_join(
      smoothed_quantiles_df,
      by = c("date", "horizon", "quantile"),
      suffix = c("_raw", "_smoothed")
    ) %>%
    dplyr::mutate(value = dplyr::coalesce(values_smoothed, values_raw)) %>%
    dplyr::select(
      date,
      quantile,
      horizon,
      values_smoothed,
      values_raw,
      value
    )

  return(smoothed_quantiles_df)
}



#' @title Calculate implied standard deviation
#'
#' @description  This function calculates the implied standard
#' deviation from normal distribution from one data point
#'
#' @param data_points_vec vector of observed data points from
#'  (assumed) normal distribution
#'
#' @param percentiles_vec vector of percentiles corresponding to the
#' data points specified in \code{data_points_vec}
#'
#' @return the value of standard deviation of the normal (mean 0)
#'  distribution implied by the \code{data_points_vec} and
#'  \code{percentiles_vec} parameters
#'
calculate_implied_std_dev = function(data_points_vec,
                                     percentiles_vec){

  loss_function = function(data_points_vec_internal,
                           percentiles_vec_internal,
                           sigma){

   errors_vec = data_points_vec_internal - qnorm(p = percentiles_vec,
                                                 mean = 0,
                                                 sd = sigma)

   loss_value = sum(abs(errors_vec))

  return(loss_value)

  }

  optim_results = optimize(f = loss_function,
                           interval = c(0,10),
                           data_points_vec_internal = data_points_vec,
                           percentiles_vec_internal = percentiles_vec)

  implied_std_dev = optim_results$minimum

  return(implied_std_dev)


}
