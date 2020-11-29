#' This function estimates the skewed t dist density based on
#' empirical quantiles
#'
#' @param estimated_values
#'
#' @param quantiles

get_skewed_t_density = function(estimated_values,
                                quantiles = c(0.05,0.25,0.5,0.75,0.95)){

  match_fun = function(x) {
    dist = estimated_values - qst(
      p = quantiles,
      xi = x[1],
      omega = x[2],
      alpha = x[3],
      nu = x[4]
    )

    return(sum(dist ^ 2))


  }

  sol = optim(c(0,1,0,1),match_fun, lower=c(-Inf, 0, -Inf, 0.0000001),
              method="L-BFGS-B")

  return(sol$par)


}
