
#' @title Calculate skew t distribution fitting loss
#'
#' @description This function calculates the loss of matching estimated quantiles
#' with theoretical ones
#'
#' @importFrom sn qst
#'
#' @importFrom magrittr "%>%"
#'
#' @param estimated_quantiles
#'
#' @param estimated_values
#'
#' @param skew_t_params

skew_t_loss = function(estimated_quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95),
                       estimated_values,
                       skew_t_params) {

  if(length(estimated_quantiles) != ncol(estimated_values)){

    stop("Estimated quantiles should be the same length as estimated values")

  }

  skew_t_values = qst(
    p = estimated_quantiles,
    xi = skew_t_params[1],
    omega = skew_t_params[2],
    alpha = skew_t_params[3],
    nu = skew_t_params[4]
  )


  loss = apply(estimated_values, 1,
               function(temp_row) {(temp_row - skew_t_values) ^ 2}) %>%
    sum()


  return(loss)

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
fit_skew_t_distribution = function(quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95),
                                   values){

  estimated_mean = apply(values, 1,mean) %>%
    mean()

  estimated_sd = apply(values, 1,sd) %>%
    mean()


  estimated_t_params = c(
    xi = estimated_mean,
    omega = estimated_sd,
    alpha = 0,
    nu = 30
  )


  optimization_result = optim(par = estimated_t_params,
                              fn = skew_t_loss,
                              estimated_values = values,
                              estimated_quantiles = quantiles,
                              method = "Nelder-Mead")

  return(optimization_result$par)

}
