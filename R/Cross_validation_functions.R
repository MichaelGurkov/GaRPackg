
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
                            method = "inner_join_pca",
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
    fix_quantile_crossing()

  return(prediction_df)

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
                                win_type_expanding = FALSE,
                                ...){


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
    horizon_list = list(horizon),
    ...
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
      mutate(date = assessment_set$date)

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


