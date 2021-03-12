#' This function calculates the loss of matching estimated quantiles
#' with theoretical ones
#'
#' @importFrom sn qst
#'
#' @param estimated_quantiles
#'
#' @param estimated_values
#'
#' @param skew_t_params

skew_t_loss = function(estimated_quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95),
                       estimated_values,
                       skew_t_params) {

  if(length(estimated_quantiles) != length(estimated_values)){

    stop("Estimated quantiles should be the same length as estimated values")

  }

  skew_t_values = qst(
    p = estimated_quantiles,
    xi = skew_t_params[1],
    omega = skew_t_params[2],
    alpha = skew_t_params[3],
    nu = skew_t_params[4]
  )


  loss = sum((estimated_values - skew_t_values) ^ 2)

  return(loss)

}
