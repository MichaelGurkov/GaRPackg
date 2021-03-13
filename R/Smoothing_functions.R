
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

skew_t_loss = function(estimated_df,
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
#' @description This function fits skew t distribution based on empirical quantiles.
#' If the estimated_values supplied as a data frame the fitting is performed on all
#' the data.
#'
#' @param estimated_df data frame with quantile column
#'
run_skew_t_fitting = function(estimated_df){

  if(!length(setdiff(names(estimated_df),c("values","quantile"))) == 0){

    stop("estimated df should only have two columns : quantile and values")
  }

  estimated_mean = estimated_df %>%
    summarise(est_mean = mean(values, na.rm = TRUE)) %>%
    pull(est_mean)

  estimated_sd = estimated_df %>%
    summarise(est_sd = sd(values, na.rm = TRUE)) %>%
    pull(est_sd)


  initial_params = c(
    xi = estimated_mean,
    omega = estimated_sd,
    alpha = 0,
    nu = 30
  )


  optimization_result = optim(par = initial_params,
                              fn = skew_t_loss,
                              estimated_df = estimated_df,
                              method = "Nelder-Mead")

  return(optimization_result$par)

}

#' @title Fit skew t distribution
#'
#' @description This function fits skew t distribution based on empirical quantiles.
#' If the estimated_values supplied as a data frame the fitting is performed on all
#' the data.
#' @param quantiles
#'
#' @param values
#'
fit_skew_t_distribution = function(estimated_df, time_limit = 10){

  if(!length(setdiff(names(estimated_df),c("values","quantile"))) == 0){

    stop("estimated df should only have two columns : quantile and values")
  }

  setTimeLimit(cpu = time_limit, elapsed = time_limit, transient = TRUE)
  on.exit({
    setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
  })

  tryCatch({
    run_skew_t_fitting(estimated_df)
  }, error = function(e) {
    if (grepl("reached elapsed time limit|reached CPU time limit", e$message)) {
      warning("t skew fitting has timed out")
      return(NA)
    } else {
      # error not related to timeout
      stop(e)
    }
  })


}
