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
#' @param method string a method that aggregates the data to partitions
#'
#' @param horizon_list list of forecast horizon
#'
#' @param quantile_vec vector of required quantiles in quantile regression
#' (corresponds to tau argument in rq)
#'
#'
run.GaR.analysis = function(partitions_list, vars_df,
                            target_var_name,
                            horizon_list,
                            quantile_vec,
                            method = "inner_join_PCA",
                            run_ols_reg = FALSE,
                            rq_method = "br",
                            pca.align.list = NULL,
                            return_objects_list = TRUE){


  # Preprocess

  preproc_df_list = reduce_data_dimension(vars_df = df,
                               pca_align_list = pca.align.list,
                               partition = partitions_list,
                               return_objects_list = return_objects_list)


  # Add lead values of target var

  reg_df = vars_df %>%
    select(Date, all_of(target_var_name)) %>%
    inner_join(preproc_df_list$xreg_df, by = c("Date" = "date")) %>%
    filter(complete.cases(.)) %>%
    add_leads_to_target_var(target_var_name = target_var_name,
                            leads_vector = unlist(horizon_list))


  # Run quantile regression

  qreg_result = lapply(horizon_list, function(temp_horizon){

    dep_var = paste(target_var_name, temp_horizon, sep = "_")

    qreg_list = rq(formula = formula(paste0(dep_var,"~.")),
                   tau = quantile_vec,
                   data = reg_df %>%
                     select(-Date) %>%
                     select(-contains(target_var_name), all_of(dep_var)),
                   method = rq_method)

    return(qreg_list)


  })

  names(qreg_result) = horizon_list


  # Run OLS regresion

  if(run_ols_reg){

    ols_result = lapply(horizon_list, function(temp_horizon){

      ols_reg = lm(formula = formula(paste0(dep_var,"~.")),
                     data = reg_df %>%
                     select(-Date) %>%
                     select(-contains(target_var_name), all_of(dep_var)))

      return(ols_reg)


    })

    names(ols_result) = horizon_list




  }


  # Check for objects and return list

  return_list = list()

  return_list$reg_df = reg_df

  return_list$qreg_result = qreg_result

  if(return_objects_list){return_list$pca_obj = preproc_df_list$objects_list}

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

#' Calculate rolling window predictions
#'
#' This function calculates the rolling regression predictions
#' for quantile regression
#'
#' @param reg_df dataframe
#'
#' @param win_type categorical can be either expanding or fixed.
#' Default is fixed
#'
#' @param win_len rolling window length
#'
#' @param quantile_vec vector of required quantiles
#'
#' @param out_of_sample_step forecasting horizon (default is next period)
#'
#' @param mod_formula model formula (default is regress the first variable
#' on everything else)
#'
#' @return A data frame with two columns: dates and predictions.
#' Column Date is the date of the out of sample data point (determined by \code{out_of_sample_step})

rolling.qreq = function(reg_df, win_len, quantile_vec,
                        out_of_sample_step = 1,
                        win_type = "fixed", mod_formula = NULL){

  if(is.null(mod_formula)){mod_formula = paste0(names(reg_df[1])," ~.")}

  n_row = nrow(reg_df)

  dates_vec = reg_df$Date

  reg_df = reg_df %>%
    select(-Date)


  rolling_grid = make.rolling.window.grid(total_len = nrow(reg_df),
                                          win_len = win_len,
                                          out_of_sample_step = out_of_sample_step,
                                          win_type = win_type)

  out_of_sample_list = lapply(rolling_grid, function(temp_ind_df){

      temp_qreq = rq(formula = formula(mod_formula),
                     tau = quantile_vec,
                     data = slice(reg_df,temp_ind_df$First:temp_ind_df$Last))

      temp_pred = predict(object = temp_qreq,
                          newdata = slice(reg_df,temp_ind_df$Last + out_of_sample_step))


      return(data.frame(Date = dates_vec[temp_ind_df$Last + out_of_sample_step],
                        temp_pred))


    })

  temp_res = out_of_sample_list %>%
    bind_rows() %>%
    filter(complete.cases(.)) %>%
    rename_at(.vars = vars(starts_with("tau")),
              .funs = list(~str_replace(.,pattern = "tau..",
                                        replacement = "")))


  return(temp_res)


}


#' @title  Calculate forecasts for GaR object
#'
#' @description This is a convenient function that calculates in sample and out of sample
#' forecast.
#' @details The in sample forecast is simply the fitted values of the regression.
#' An out of sample forecast is calculated by rolling regression (determined by \code{win_len}) that forecast
#' ahead according to \code{out_of_sample_step}
#'
get.gar.forecast = function(gar_obj, win_len, quantile_vec,
                            out_of_sample_step = 1,
                            win_type = "fixed"){

  # Calculate in sample forecast

  forecast_in_sample = lapply(names(gar_obj$qreg_result),
                              function(temp_name){

                                    temp_reg = gar_obj$qreg_result[[temp_name]]

                                    col_names = gar_obj$qreg_result[[1]] %>%
                                      coefficients() %>%
                                      colnames() %>%
                                      gsub(pattern = "tau= ",replacement = "")

                                    temp_reg$fitted.values %>%
                                      as.data.frame() %>%
                                      setNames(col_names) %>%
                                      mutate(Date = gar_obj$reg_df$Date[1:nrow(.)]) %>%
                                      gather(key = Quantile,value = GaR_forecast, -Date) %>%
                                      mutate(Horizon = temp_name)

                                  }) %>%
    bind_rows() %>%
    mutate(Date = as.yearqtr(Date)) %>%
    mutate(Forecast_Status = "In Sample")

  # Calculate out of sample forecast

  forecast_out_of_sample = lapply(parameters_list$horizon_list,
                                      function(temp_horizon){

                                        temp_var = paste("GDP_growth",temp_horizon,sep = "_")

                                        temp_roll = rolling.qreq(reg_df = gar_obj$reg_df  %>%
                                                                   select(names(.)[!grepl("GDP",names(.))],
                                                                          all_of(temp_var)),
                                                                 win_len = win_len,
                                                                 quantile_vec = quantile_vec,
                                                                 mod_formula = paste0(temp_var," ~ ."),
                                                                 out_of_sample_step = out_of_sample_step,
                                                                 win_type = win_type)

                                        temp_roll = temp_roll %>%
                                          mutate(Horizon = as.character(temp_horizon))

                                        temp_roll = temp_roll %>%
                                          filter(complete.cases(.))

                                        return(temp_roll)

                                      }) %>%
    bind_rows() %>%
    mutate(Date = as.yearqtr(Date)) %>%
    gather(key = Quantile, value = GaR_forecast, -Date, -Horizon) %>%
    select(Date, Quantile, GaR_forecast, Horizon) %>%
    mutate(Forecast_Status = "Out of Sample")


  res_df = list(forecast_in_sample, forecast_out_of_sample) %>%
    bind_rows() %>%
    mutate(Date = as.yearqtr(Date))


  return(res_df)


}



#' @title Calculate Osnat's quantile fit score
#'
#' @description This function evaluates the goodness of fit between
#' "realized" and to forecasted CDF
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


#' @title Make rolling window indices
#'
#' @description The function makes a grid of rolling window indices
#'
#' @param total_len = length of full grid (number of rows in data frame)
#'
#' @param win_len length of rolling window
#'
#' @param out_of_sample_step forecasting horizon
#'
#' @param win_type categorical can be either expanding or fixed. Default is fixed
#'

make.rolling.window.grid = function(total_len ,win_len,
                                    out_of_sample_step,
                                    win_type = "fixed"){

  if(win_type == "fixed"){

    rolling_grid = lapply(1:(total_len - win_len + 1),
                              function(temp_ind){

                                first_ind = temp_ind

                                last_ind = win_len + temp_ind - 1

                                return(data.frame(First = first_ind, Last = last_ind))


                              })

  } else if (win_type == "expanding"){

    rolling_grid = lapply(1:(total_len - win_len + 1),
                          function(temp_ind){

                            first_ind = 1

                            last_ind = win_len + temp_ind - 1

                            return(data.frame(First = first_ind, Last = last_ind))


                          })

  }


  return(rolling_grid)






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

