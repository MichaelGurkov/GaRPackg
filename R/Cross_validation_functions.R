
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
#' @param win_len the length of sliding window (default 30).
#'  Can be supplied as start date of the analysis (test) set
#'
#' @param win_type_expanding boolean should the sliding window expand
#'  (default TRUE)
#'
#' @return a data frame with out-of-sample predictions.
#' The "date" column specifies the date of the last observation in the test set,
#'  namely, the date of the last observation used to construct the forecast.
#'  The "forecast_target_date" specifies the date for which the forecast is
#'   aimed, namely, forecast_target_date = date + horizon.
#'
#' @export
#'

get_gar_forecast = function(partitions_list,
                            vars_df,
                            target_var_name,
                            horizon_list,
                            quantile_vec,
                            pca.align.list = NULL,
                            preprocess_method = "pca",
                            win_len = 30,
                            win_type_expanding = TRUE){


  prediction_df = run_cross_validation(
    partitions_list = partitions_list,
    vars_df = vars_df,
    target_var_name = target_var_name,
    horizon_list = horizon_list,
    quantile_vec = quantile_vec,
    preprocess_method = preprocess_method,
    pca.align.list = pca.align.list,
    win_len = win_len,
    win_type_expanding = win_type_expanding) %>%
    fix_quantile_crossing()

  frequency = identify_frequency(prediction_df$date)

  if(frequency == "quarterly"){

    prediction_df = prediction_df %>%
      dplyr::mutate(forecast_target_date = as.yearqtr(date) + as.numeric(horizon) / 4)

  }

  if (frequency == "monthly"){

    prediction_df = prediction_df %>%
      dplyr::mutate(forecast_target_date = as.yearmon(date) + as.numeric(horizon) / 12)
  }

  prediction_df = prediction_df %>%
    dplyr::relocate("forecast_target_date",.after = "forecast_values")


  return(prediction_df)

}




#' @title Run cross validation for quantile regression
#'
#' @description  The function performs rolling ("walk forward")
#' quantile regression.
#'
#' @details After the estimation of the parameters on
#' the rolling window sample prediction for last observation is performed.
#' This is and "out of sample" prediction since the last observation data was
#' not used in the estimation of the parameters (due to unknown target feature)
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
#' @param win_len the length of sliding window (default 30).
#'  Can be supplied as start date of the analysis (test) set
#'
#' @param win_type_expanding boolean should the sliding window expand
#'  (default TRUE)

#'
#'
#' @import tidyr
#'
#' @importFrom stringr str_remove
#'
#' @importFrom stats predict
#'
#'
run_cross_validation = function(partitions_list,
                                vars_df,
                                target_var_name,
                                horizon_list,
                                quantile_vec,
                                pca.align.list = NULL,
                                preprocess_method = "pca",
                                win_len = 30,
                                win_type_expanding = TRUE,
                                ...){
if(!is.numeric(win_len)){

  win_len = calculate_win_len_from_date(start_date = win_len,
                                        data_df = vars_df)

}

roll_cv_list = vars_df %>%
  rsample::rolling_origin(
    initial = win_len,
    assess = 0,
    cumulative = win_type_expanding
  )

predict_df = purrr::map(roll_cv_list$splits,
                        function(temp_split) {

                          analysis_set = make_quant_reg_df(
                            partitions_list = partitions_list,
                            vars_df = analysis(temp_split),
                            target_var_name = target_var_name,
                            horizon_list = horizon_list,
                            pca.align.list = pca.align.list,
                            preprocess_method = preprocess_method,
                            return_objects_list = FALSE
                          )

                          analysis_set = analysis_set[["reg_df"]]

                          assessment_set = analysis_set %>%
                            dplyr::slice(n())

                          qreg_result = run_quant_reg(
                            reg_df = analysis_set,
                            target_var_name = target_var_name,
                            quantile_vec = quantile_vec,
                            horizon_list = horizon_list
                          )



                          temp_predict = purrr::map(names(qreg_result), function(temp_name) {
                            temp_pred = qreg_result[[temp_name]] %>%
                              stats::predict(newdata = assessment_set) %>%
                              as.data.frame() %>%
                              dplyr::rename_all( ~ str_remove(., "tau= ")) %>%
                              tidyr::pivot_longer(cols = everything(),
                                                  names_to = "quantile",
                                                  values_to = "forecast_values") %>%
                              dplyr::mutate(horizon = temp_name) %>%
                              dplyr::mutate(date = assessment_set$date)

                            if (length(quantile_vec) == 1) {
                              temp_pred = temp_pred %>%
                                dplyr::mutate(quantile = quantile_vec)
                            }

                            return(temp_pred)




                          }) %>%
                            dplyr::bind_rows()



                        }) %>%
  dplyr::bind_rows()

return(predict_df)

}


