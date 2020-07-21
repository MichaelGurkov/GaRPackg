#' @title Perform GaR analysis
#'
#' @description This is a convenient function that encapsulates the GaR analysis.
#' The stages of the analysis are :
#' \itemize{
#'   \item Perform PCA
#'   \item Run quantile regression
#' }
#'
#'
#' @importFrom stats formula
#'
#' @param partititions_list list of partitons
#'
#' @param vars_df data frame with input variables
#'
#' @param target_var_name string that specifies outcome feature
#'
#' @param horizon_list list of forecast horizon
#'
#' @param quantile_vec vector of required quantiles in quantile
#' regression (corresponds to tau argument in rq)
#'
#' @param method string a method that aggregates the data to partitions
#'
#' @param run_ols_reg boolean indicator that adds an OLS regression
#'
#'
#' @param pca.align.list A list that specifies the PCA aligning variable for
#' each partition and alignment direction (default is positive direction).
#'
#' @param return_objects_list boolean indicator that returns PCA objects.
#' Default is TRUE
#'
run.GaR.analysis = function(partitions_list, vars_df,
                            target_var_name,
                            horizon_list,
                            quantile_vec,
                            method = "inner_join_PCA",
                            run_ols_reg = FALSE,
                            pca.align.list = NULL,
                            return_objects_list = TRUE){


  reg_df_list = make.quant.reg.df(
    partitions_list = partitions_list,
    vars_df = vars_df,
    target_var_name = target_var_name,
    horizon_list = horizon_list,
    quantile_vec = quantile_vec,
    pca.align.list = pca.align.list,
    method = method,
    return_objects_list = return_objects_list
    )



  qreg_result = run.quant.reg(
    reg_df = reg_df_list$reg_df,
    target_var_name = target_var_name,
    quantile_vec = quantile_vec,
    horizon_list = horizon_list
    )



  # Run OLS regresion

  if(run_ols_reg){

    ols_result = lapply(horizon_list, function(temp_horizon){

      ols_reg = lm(formula = formula(paste0(dep_var,"~.")),
                     data = reg_df_list$reg_df %>%
                     select(-Date) %>%
                     select(-contains(target_var_name), all_of(dep_var)))

      return(ols_reg)


    })

    names(ols_result) = horizon_list

  }


  # Check for objects and return list

  return_list = list()

  return_list$reg_df = reg_df_list$reg_df

  return_list$qreg_result = qreg_result

  if(length(reg_df_list) == 2){
    return_list$pca_obj = reg_df_list$pca_obj}

  if(run_ols_reg){return_list$ols_result = preproc_df_list$ols_result}

  return(return_list)

  }



#' This is a convinience functions that plots the coefficients of
#' quantile regression
#'
#'@param quantile_reg
#'
plot.qreg.coeffs = function(quantile_reg, print_plot = TRUE,
                            add.significance = FALSE){

  coeff_data = suppressWarnings(lapply(names(quantile_reg),
                                       function(temp_name){

    temp_summary_list = summary(quantile_reg[[temp_name]])

    coeff_data = lapply(temp_summary_list,
                        function(temp_summary){

      temp_df = data.frame(Tau = as.character(temp_summary$tau),
                           temp_summary$coefficients[-1,])

      temp_df$Name = rownames(temp_df)

      return(temp_df)

      }) %>%
      bind_rows()

    coeff_data$Horizon = temp_name

    return(coeff_data)

    })) %>%
    bind_rows() %>%
    setNames(str_to_title(names(.))) %>%
    mutate(Significant = (0 >= Upper.bd | 0 <= Lower.bd))

  for (temp_horizon in unique(coeff_data$Horizon)) {

    if(add.significance){

      temp_plot = ggplot(coeff_data %>%
                           filter(Horizon == temp_horizon),
                         aes(x = Tau, y = Coefficients,
                             fill = Significant)) +
        geom_bar(stat = "identity", width = 0.25) +
        geom_hline(yintercept = 0, linetype = "dashed") +
        scale_fill_manual(values = c("TRUE" = "lightblue",
                                     "FALSE" = "lightgray")) +
        labs(title = paste(temp_horizon, "quarters ahead")) +
        theme_bw() +
        theme(legend.position = "bottom",
              plot.title = element_text(hjust = 0.5)) +
        facet_wrap(~Name)

    } else {

      temp_plot = ggplot(coeff_data %>%
                           filter(Horizon == temp_horizon),
                         aes(x = Tau, y = Coefficients)) +
        geom_bar(stat = "identity", width = 0.25) +
        geom_hline(yintercept = 0, linetype = "dashed") +
        scale_fill_manual(values = c("TRUE" = "lightblue",
                                     "FALSE" = "lightgray")) +
        labs(title = paste(temp_horizon, "quarters ahead")) +
        theme_bw() +
        theme(legend.position = "bottom",
              plot.title = element_text(hjust = 0.5)) +
        facet_wrap(~Name)


    }

    if(print_plot){

      print(temp_plot)


    } else {

      return(temp_plot)

      }

  }


}


#' @title  Out of sample forecasts
#'
#' @description This is a convenient function that calculates out of
#' sample forecast.
#' @details An out of sample forecast is calculated by rolling
#' regression (determined by \code{win_len}) that forecast
#' ahead according to \code{out_of_sample_step}
#'
#' @import rsample
#'
#' @return a data frame with out of sample predictions
#'

get.gar.forecast = function(partitions_list,
                            vars_df,
                            target_var_name,
                            horizon_list,
                            quantile_vec,
                            pca.align.list = NULL,
                            method,
                            win_len = 30,
                            win_type_expanding = FALSE){

  reg_df_list = make.quant.reg.df(
    partitions_list = partitions_list,
    vars_df = vars_df,
    target_var_name = target_var_name,
    horizon_list = horizon_list,
    quantile_vec = quantile_vec,
    pca.align.list = pca.align.list,
    method = method,
    return_objects_list = FALSE
  )


  prediction_df = map(horizon_list,run.cross.validation,
                    reg_df = reg_df_list$reg_df,
                    target_var_name = target_var_name,
                    quantile_vec = quantile_vec,
                    win_len = win_len,
                    win_type_expanding = win_type_expanding) %>%
  bind_rows()

  return(prediction_df)

}



#' @title Calculate Osnat's quantile fit score
#'
#' @description This function evaluates the goodness of fit between
#' "realized" and to forecasted CDF
#'
#' @details
#'
quantile.fit.score = function(realized_estimate, quantile_values, quantiles){

  if(length(quantile_values) != length(quantiles)){

    stop("Quantile values and quantiles must be of same length ")

  }

  score = 0

  for (ind in seq_along(quantiles)){

    if(quantile_values[ind] < realized_estimate){

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
quantile.fit.score.area = function(realized_estimate,
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

  vec = c(vec, realized_estimate)

  vec = vec[order(vec)] # order vec (quantile values + realization)

  real_est_ind = which(vec == realized_estimate)

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
quantile.crps.score = function(realized_estimate,
                                   quantile_values,
                                   quantiles,
                                   min_quantile_value = -0.2,
                                   max_quantile_value = 0.2){

  # Data validation

  if(length(quantile_values) != length(quantiles)){

    stop("Quantile values and quantiles must be of same length ")

  }

  if(realized_estimate < min_quantile_value){

    message("Realized estimate is less than min quantile value")

    return(NA)


  }

  if(realized_estimate > max_quantile_value){

    message("Realized estimate is greater than max quantile value")

    return(NA)


  }

  # Construct named vector to hold values and quantiles
  # the names of the vector are quantiles and the values are
  # quantile values

  # Construct vector to hold values and quantiles

  vec = c(min_quantile_value,quantile_values, max_quantile_value)

  names(vec) = c(0,quantiles,1)

  vec = c(vec, realized_estimate)

  vec = vec[order(vec)] # order vec (quantile values + realization)

  real_est_ind = which(vec == realized_estimate)

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
quantile.r2.score = function(realized_estimates, forecast_values,
                             quantile, benchmark_values){

  if(length(forecast_values) != length(benchmark_values)){

    stop("Forecast values and benchmark must be of same length ")

  }


  nom = 0

  denom = 0

  for (ind in 1:length(forecast_values)){

    realized_error = realized_estimates[ind] - forecast_values[ind]

    benchmark_error = realized_estimates[ind] - benchmark_values[ind]

    nom = nom + if_else(realized_error > 0,
                        realized_error * quantile,
                        realized_error * (quantile - 1))

    denom = denom + if_else(benchmark_error > 0,
                            benchmark_error * quantile,
                            benchmark_error * (quantile - 1))


  }

  return(1 - nom / denom)



}


#' This function runs quantile regression
#'
#'
run.quant.reg = function(reg_df,
                         target_var_name,
                         quantile_vec,
                         horizon_list){

  qreg_result = map(horizon_list, function(temp_horizon){

    dep_var = paste(target_var_name, temp_horizon, sep = "_")

    qreg_list = rq(formula = formula(paste0(dep_var,"~.")),
                   tau = quantile_vec,
                   data = reg_df %>%
                     select(-Date) %>%
                     select(-contains(target_var_name), all_of(dep_var)))

    return(qreg_list)


  })

  names(qreg_result) = horizon_list


  return(qreg_result)


}


#' @title Run cross validation for quantile regression
#'
#' @description  The function performs rolling ("walk forward")
#' quantile regression. After the estimation of the parameters on
#' the rolling window sample an out of sample prediction is performed
#' based on the out of sample step that is determined by the
#'  \code{horizon} parameter.
#'
#' @param reg_df data
#'
#' @param target_var_name string that specifies outcome feature
#'
#' @param horizon determines the out of sample step
#'
#' @param quantile_vec vector of required quantiles in quantile
#' regression (corresponds to tau argument in rq)
#'
#' @param win_len rolling window length
#'
#' @param win_type_expanding boolean indicator that determines
#' whether the rolling window should be expanding or rolling
#'
#'
run.cross.validation = function(reg_df,
                                target_var_name,
                                horizon,
                                quantile_vec,
                                win_len = 30,
                                win_type_expanding = FALSE){


roll_cv_list = reg_df %>%
  rolling_origin(
    initial = win_len,
    assess = horizon,
    cumulative = win_type_expanding
  )

predict_df = map(roll_cv_list$splits,
                 function(temp_split){

  analysis_set = analysis(temp_split)

  assessment_set = assessment(temp_split) %>%
    slice(n())

  qreg_result = run.quant.reg(
    reg_df = analysis_set,
    target_var_name = target_var_name,
    quantile_vec = quantile_vec,
    horizon_list = list(horizon)
  )

  temp_predict = map(names(qreg_result), function(temp_name){

    temp_pred = qreg_result[[temp_name]] %>%
      predict(newdata = assessment_set) %>%
      as.data.frame() %>%
      rename_all(~str_remove(.,"tau= ")) %>%
      pivot_longer(cols = everything(),
                   names_to = "Quantile",
                   values_to = "GaR_forecast") %>%
      mutate(Horizon = temp_name) %>%
      mutate(Date = assessment_set$Date)

    return(temp_pred)

  }) %>%
    bind_rows()



}) %>%
  bind_rows()

return(predict_df)

}
