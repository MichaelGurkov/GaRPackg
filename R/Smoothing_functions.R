
#' @title Calculate skew t distribution fitting loss
#'
#' @description This function calculates the loss of matching estimated quantiles
#' with theoretical ones
#'
#' @importFrom sn qst
#'
#' @importFrom magrittr "%>%"
#'
#' @param estimated_df data frame with quantile column
#'
#' @param skew_t_params

t_skew_loss = function(estimated_df,
                       skew_t_params) {

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
    summarise(loss = sum(error ^ 2)) %>%
    pull(loss)


  return(loss)

}



#' @title Fit skew t distribution
#'
#' @description This function fits t skew distribution based on empirical quantiles.
#' If the estimated_values supplied as a data frame the fitting is performed on all
#' the data.
#'
#' @param estimated_df_x data frame with quantile column
#'
#' @param bounded_optimization_x boolean indicator, default TRUE
#'
#' @param lower_bounds_x lower bounds for xi (location),
#' omega (scale), alpha (slant), nu (degrees of freedom).
#' The default is (-Inf, 0, -Inf, 0).
#'
#' @param upper_bounds_x upper bounds for xi (location),
#' omega (scale), alpha (slant), nu (degrees of freedom).
#' The default is (Inf, Inf, Inf, 100).
#'
run_t_skew_fitting = function(estimated_df_x,
                              bounded_optimization_x = TRUE,
                              lower_bounds_x = c(-Inf, 0, -Inf, 0),
                              upper_bounds_x = c(Inf, Inf, Inf, 100)){

  if(!length(setdiff(names(estimated_df_x),c("values","quantile"))) == 0){

    stop("estimated df should only have two columns : quantile and values")
  }

  estimated_mean = estimated_df_x %>%
    summarise(est_mean = mean(values, na.rm = TRUE)) %>%
    pull(est_mean)

  estimated_sd = estimated_df_x %>%
    summarise(est_sd = sd(values, na.rm = TRUE)) %>%
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


#' @title Fit skew t distribution
#'
#' @description This function fits skew t distribution based on empirical quantiles.
#' If the estimated_values supplied as a data frame the fitting is performed on all
#' the data.
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
#' @export
#'
fit_skew_t_distribution = function(estimated_df,
                                   time_limit = 10,
                                   bounded_optimization = TRUE,
                                   lower_bounds = c(-Inf, 0, -Inf, 0),
                                   upper_bounds = c(Inf, Inf, Inf, 100)){

  if(!length(setdiff(names(estimated_df),c("values","quantile"))) == 0){

    stop("estimated df should only have two columns : quantile and values")
  }

  setTimeLimit(cpu = time_limit, elapsed = time_limit, transient = TRUE)
  on.exit({
    setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
  })

  tryCatch({
    run_t_skew_fitting(estimated_df_x = estimated_df,
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

