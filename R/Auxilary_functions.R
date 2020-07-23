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
#' @import purrr
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
    bind_rows() %>%
    fix.quantile.crossing()

  return(prediction_df)

}





#' This function runs quantile regression
#'
#'@import quantreg
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
#' @import tidyr
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

    if(length(quantile_vec) == 1){
      temp_pred = temp_pred %>%
        mutate(Quantile = quantile_vec)
      }

    return(temp_pred)




  }) %>%
    bind_rows()



}) %>%
  bind_rows()

return(predict_df)

}


#' @title Fixes quantile regression crossing
#'
#' This function fixes the crossing issue in quantile regression
#'
#' @import tidyr
#'
#' @param prediction_df
#'
fix.quantile.crossing = function(prediction_df){

   prediction_df = prediction_df %>%
    group_by(Horizon,Date) %>%
    arrange(Quantile) %>%
    mutate(GaR_forecast = sort(GaR_forecast)) %>%
    ungroup()


   return(prediction_df)




}
