
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
#' @param estimated_df data frame with (quantiles, values) columns
#'
#' @param skew_t_params

t_skew_loss = function(estimated_df, skew_t_params) {

  quantiles_vec = unique(estimated_df$quantile)

  skew_t_values = qst(
    p = quantiles_vec,
    xi = skew_t_params[1],
    omega = skew_t_params[2],
    alpha = skew_t_params[3],
    nu = skew_t_params[4]
  )

  skew_t_df = tibble(quantile = quantiles_vec,
                     skew_t_values = skew_t_values)

  loss = estimated_df %>%
    left_join(skew_t_df, by = "quantile") %>%
    mutate(error = values - skew_t_values) %>%
    summarise(loss = sqrt(mean(error ^ 2))) %>%
    pull(loss)


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
                              lower_bounds_x = c(-Inf, 0, -Inf, 1),
                              upper_bounds_x = c(Inf, Inf, Inf, 100)){

  if(!length(setdiff(names(estimated_df_x),c("values","quantile"))) == 0){

    stop("estimated df should only have two columns : quantile and values")
  }

  estimated_mean = estimated_df_x %>%
    summarise(est_mean = mean(.data$values, na.rm = TRUE)) %>%
    pull(est_mean)

  estimated_sd = estimated_df_x %>%
    summarise(est_sd = sd(.data$values, na.rm = TRUE)) %>%
    pull(est_sd)


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
                                fn = skew_t_loss,
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
#' @param time_limit the time limit given to each optimization round.
#'  In case the computation doesn't converge in the given time frame a vector
#'  of 0 is returned for the optimization parameters and "timed out" warning is
#'  issued.
#'
#' @export
#'
fit_t_skew = function(estimated_df,time_limit = 10,
                                   bounded_optimization = TRUE,
                                   lower_bounds = c(-Inf, 0, -Inf, 1),
                                   upper_bounds = c(Inf, Inf, Inf, 100)){

  # Validation

  if(!length(setdiff(names(estimated_df),c("values","quantile"))) == 0){

    stop("estimated df should only have two columns : quantile and values")
  }

  col_class = map_chr(estimated_df, class)

  non_numeric_vars = names(col_class)[!col_class == "numeric"]

  if(length(non_numeric_vars) > 0){

    warning(paste("Column(s)",paste(non_numeric_vars, collapse = ","),
                  "have been converted to numeric"))

    estimated_df = estimated_df %>%
      mutate(across(all_of(non_numeric_vars), as.numeric))


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

#' @title Smooth gar forecast with skew t distribution
#'
#' @description
#'
#' @param gar_forecast_df
#'
#' @param time_limit
#'
#' @export
#'
extract_t_skew_from_gar_forecast = function(gar_forecast_df,
                                            time_limit = 10,
                                            bounded_optimization = TRUE,
                                            lower_bounds = c(-Inf, 0, -Inf, 1),
                                            upper_bounds = c(Inf, Inf, Inf, 100)) {

  required_cols = c("forecast_values","quantile","horizon","date")

  if (length(setdiff(required_cols,names(gar_forecast_df))) > 0) {

    stop(paste0("The following column(s) are missing:",
                paste0(setdiff(required_cols,names(gar_forecast_df)),
                       collapse = ",")))

  }

  t_skew_fit_df = gar_forecast_df %>%
    rename(values = forecast_values) %>%
    group_by(date, horizon) %>%
    group_map(function(temp_df, temp_name){

      fit_params =  fit_t_skew(select(temp_df, c(
        "quantile", "values")),time_limit = time_limit)

      fit_params_df = tibble(temp_name,parameter = names(fit_params), value = fit_params)

      return(fit_params_df)

    }) %>%
    bind_rows()



  return(t_skew_fit_df)





}
