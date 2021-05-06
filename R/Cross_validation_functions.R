
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
#'
#' @param partitions_list list of partition names
#'
#' @param vars_df data frame with input variables
#'
#' @param target_var_name string that specifies outcome variable
#'
#' @param horizon_list list (or vector) of forecast horizons
#'
#' @param quantile_vec vector quantiles for quantile regression
#'
#' @param pca.align.list (optional) list list that contains the "aligning" variable
#' for each partition that should be "aligned". See
#'
#' @param preprocess_method dimension reduction method (default PCA)
#'
#' @param win_len the length of sliding window (default 30)
#'
#' @param win_type_expanding boolean should the sliding window expand
#'  (default TRUE)
#'
#' @return a data frame with out of sample predictions.
#' The date column specifies the date of the test set observation,
#' the target date is the date + horizon * 0.25 (the date units are quarters)
#'
#' @export
#'

get_gar_forecast = function(partitions_list,
                            vars_df,
                            target_var_name,
                            horizon_list,
                            quantile_vec,
                            pca.align.list = NULL,
                            preprocess_method = "inner_join_pca",
                            win_len = 30,
                            win_type_expanding = TRUE){

  reg_df_list = make_quant_reg_df(
    partitions_list = partitions_list,
    vars_df = vars_df,
    target_var_name = target_var_name,
    horizon_list = horizon_list,
    pca.align.list = pca.align.list,
    preprocess_method = preprocess_method,
    return_objects_list = FALSE
  )

  if(nrow(reg_df_list$reg_df) == 0){stop("The regression data frame is empty")}


  prediction_df = map(horizon_list,run_cross_validation,
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
#' @param win_type_expanding  boolean should the sliding window expand
#'  (default TRUE)
#'
#' @param ... external optional arguments
#'
#'
#'
#' @import tidyr
#'
#' @importFrom stringr str_remove
#'
#' @importFrom stats predict
#'
#'
run_cross_validation = function(reg_df,
                                target_var_name,
                                horizon,
                                quantile_vec,
                                win_len = 30,
                                win_type_expanding = TRUE,
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

  qreg_result = run_quant_reg(
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
                   names_to = "quantile",
                   values_to = "forecast_values") %>%
      mutate(horizon = temp_name) %>%
      mutate(date = assessment_set$date)

    if(length(quantile_vec) == 1){
      temp_pred = temp_pred %>%
        mutate(quantile = quantile_vec)
      }

    return(temp_pred)




  }) %>%
    bind_rows()



}) %>%
  bind_rows()

return(predict_df)

}


