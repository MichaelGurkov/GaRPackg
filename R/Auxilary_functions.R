
#' @description This function extracts the coefficients from quantile regression
#'
#' @title Extract coefficients from quantile reg
#'
#' @importFrom rlang .data
#'
#'
#' @param qreg_obj quantile regression object
#'
#'
extract_qreq_coeff_table = function(qreg_obj) {
  coef_table = lapply(suppressWarnings(summary(qreg_obj)),
                      function(temp_list) {
                        temp_df = as.data.frame(temp_list$coefficients)

                        temp_df$tau = temp_list$tau

                        temp_df$partition = rownames(temp_df)

                        rownames(temp_df) = NULL

                        return(temp_df)

                      }) %>%
    dplyr::bind_rows() %>%
    dplyr::rename(
      coeff = .data$coefficients,
      low = .data$`lower bd`,
      high = .data$`upper bd`,
      quantile = .data$tau
    ) %>%
    dplyr::mutate(quantile = as.character(.data$quantile)) %>%
    dplyr::mutate(partition = gsub("(Intercept)", "Intercept", .data$partition, fixed = TRUE)) %>%
    dplyr::mutate(significant = factor(
      ifelse(.data$high <= 0 | .data$low >= 0, "significant",
             "non_significant"),
      levels = c("significant", "non_significant")
    ))

  return(coef_table)

}


#' @description This function extracts the coefficients data frame from gar model
#'
#' @title Extract regression coefficient from gar model object
#'
#' @importFrom stringr str_remove_all
#'
#' @importFrom rlang .data
#'
#' @param gar_model model object with run_GaR_analysis result
#'
#' @param partition_names optional which partitions to return
#'
#' @return coeffs_df
#'
#' @export

extract_coeffs_from_gar_model = function(gar_model,
                                         partition_names = NULL) {
  stopifnot("qreg_result" %in% names(gar_model))

  coeffs_df = gar_model[["qreg_result"]] %>%
    purrr::map_dfr(extract_qreq_coeff_table,.id = "horizon") %>%
    dplyr::relocate(.data$partition, .data$horizon, .data$quantile,
             .data$coeff, .data$low, .data$high, .data$significant) %>%
    dplyr::mutate(partition = stringr::str_remove_all(.data$partition,"_xreg$"))

  if (!is.null(partition_names)) {
    coeffs_df = coeffs_df %>%
      dplyr::filter(.data$partition %in% partition_names)


  }

  return(coeffs_df)


}


#' @title Extract factor contribution from gar model
#'
#' @details This function extracts the factor contribution (coefficients
#'  multiplied by values) data frame from gar model for given quantile
#'
#' @import purrr
#'
#' @importFrom magrittr %>%
#'
#' @importFrom rlang .data
#'
#' @importFrom stringr str_remove_all
#'
#' @param gar_model model object with run_GaR_analysis result
#'
#' @param target_quantile dplyr::filtering quantile (default 0.05)
#'
#' @return factor_contribution_df
#'
#' @export

extract_factor_contribution_from_gar_model = function(
  gar_model, target_quantile = "0.05") {

  stopifnot("qreg_result" %in% names(gar_model))

  if(!target_quantile %in% gar_model$qreg_result[[1]]$tau){

    stop("The target quantile is not in estimated quantiles")
  }

  data_mat = gar_model$reg_df %>%
    dplyr::select(dplyr::ends_with("_xreg")) %>%
    as.matrix()

  data_mat = cbind(rep(1,nrow(data_mat)),data_mat)


  coeffs_df = gar_model %>%
    extract_coeffs_from_gar_model() %>%
    dplyr::filter(.data$quantile == target_quantile) %>%
    dplyr::select(.data$coeff,.data$horizon, .data$partition)



  factors_df = purrr::map_dfr(
    unique(coeffs_df$horizon),function(temp_horizon){

      coef_vec = coeffs_df %>%
        dplyr::filter(.data$horizon == temp_horizon) %>%
        dplyr::select(.data$coeff) %>%
        unlist(use.names = FALSE)

      factors_df =  t(t(data_mat) * coef_vec)

      factors_df = factors_df %>%
        as.data.frame() %>%
        cbind(date = gar_model$reg_df$date) %>%
        dplyr::mutate(horizon = temp_horizon)

      return(factors_df)

    })

  factors_df = factors_df %>%
    dplyr::rename_all(~stringr::str_remove_all(.,"_xreg")) %>%
    dplyr::rename(intercept = .data$V1)

  return(factors_df)

}


#' @title Calculate skew and IQR measures
#'
#' @details This function takes gar object, extracts
#' predictions and calculates Skew and IQR measures
#'
#' @param gar_obj model object with run_GaR_analysis result
#'
#' @param quantile_values vector that specifies the quantiles used in the calculation
#' of skew and iqr. The structure of the vector should be (low,mid,high), for example
#' if the iqr is based on 95th and 5th quantile the vector will be c(0.05,0.5,0.95).
#'
#' @importFrom rlang .data
#'
#' @export
#'
calculate_skew_and_iqr = function(gar_obj,
                                  quantile_values = c("0.25","0.5","0.75")) {
  quantile = NULL

  quantile_names = c("low","mid","high")

  rename_table = tibble::tibble(quantile = as.numeric(quantile_values),
                        names = quantile_names)


  prediction_df = make_prediction_df(gar_model = gar_obj$qreg_result,
                                     xreg_df = gar_obj$reg_df)

  missing_quantiles = setdiff(quantile_values,unique(prediction_df$quantile))

  if(!length(missing_quantiles) == 0){

    stop(paste("the following quantile(s) are missing in the model object:",
               paste(missing_quantiles, collapse = ",")))

  }

  skew_df = prediction_df %>%
    dplyr::inner_join(rename_table, by = "quantile") %>%
    dplyr::select(-quantile) %>%
    tidyr::pivot_wider(
      names_from = .data$names,
      values_from = .data$fitted_values
    ) %>%
    dplyr::mutate(skew = (0.5 * .data$high + 0.5 * .data$low - .data$mid) /
             (0.5 * .data$high - 0.5 * .data$low)) %>%
    dplyr::mutate(iqr = .data$high - .data$low) %>%
    dplyr::select(.data$date,.data$horizon,.data$skew,.data$iqr)

  return(skew_df)


}



#' @title Extract PCA loadings
#'
#' @description This function extracts PCA loadings data frame from gar model
#'
#' @importFrom  purrr map_dfr
#'
#' @importFrom tibble rownames_to_column
#'
#' @importFrom magrittr %>%
#'
#' @importFrom stats setNames
#'
#' @param gar_model model object with run_GaR_analysis result
#'
#' @return pca_loadings_df
#'
#' @export

extract_pca_loadings_from_gar_model = function(gar_model) {

  if(!"pca_obj" %in% names(gar_model)){

    stop(paste("The pca object is missing.",
               "Perhaps all the partitions are one variable only?"))
  }

  pca_loadings_df = purrr::map_dfr(gar_model$pca_obj, function(temp_pca) {
    temp_coeffs = temp_pca$pca_obj$rotation[, 1] %>%
      as.data.frame() %>%
      setNames("coeff") %>%
      tibble::rownames_to_column()


  },  .id = "partition")

  return(pca_loadings_df)

}


#' @title Extract PCA timeseries
#'
#' @description This function extracts PCA timeseries data frame from gar model
#'
#' @importFrom  purrr map_dfr
#'
#' @importFrom tibble rownames_to_column
#'
#' @importFrom stats setNames
#'
#' @importFrom magrittr %>%
#'
#' @param gar_model model object with run_GaR_analysis result
#'
#' @param n_comp number of PCA components to return
#'
#' @return pca_timeseries_df
#'
#' @export

extract_pca_timeseries_from_gar_model = function(gar_model, n_comp = 1) {

  if(!"pca_obj" %in% names(gar_model)){

    stop("The pca object is missing")
  }

  pca_loadings_df = purrr::map2(gar_model$pca_obj, names(gar_model$pca_obj),
                             function(temp_pca, temp_name) {

    temp_pca_df = temp_pca$pca_obj$x[, 1:n_comp] %>%
      as.data.frame() %>%
      cbind(date = temp_pca$time_index) %>%
      dplyr::relocate(date)

    if (n_comp > 1) {
      temp_pca_df = temp_pca_df %>%
        setNames(c("date",paste(temp_name, 1:n_comp, sep = "_")))

    } else {
      temp_pca_df = temp_pca_df %>%
        setNames(c("date",temp_name))

    }

    return(temp_pca_df)


  }) %>%
    purrr::reduce(dplyr::full_join, by = "date")


  return(pca_loadings_df)

}


#' @title Extract PCA explained variance share
#'
#' @description This function extracts PCA explained variance share data frame
#'  from gar model
#'
#' @importFrom  purrr map_dfr
#'
#' @importFrom tibble rownames_to_column
#'
#' @importFrom stats setNames
#'
#' @importFrom magrittr %>%
#'
#' @param gar_model model object with run_GaR_analysis result
#'
#' @param n_comp number of PCA components to return
#'
#' @return pca explained variance share
#'
#' @export

extract_pca_exlained_variance_from_gar_model = function(gar_model, n_comp = 1) {

  explained_variance = NULL

  if(!"pca_obj" %in% names(gar_model)){

    stop("The pca object is missing")
  }

  pca_expained_var_df = purrr::map2_dfr(gar_model$pca_obj, names(gar_model$pca_obj),
                         function(temp_pca, temp_name) {

                           temp_pca_share = tibble::tibble(
                             explained_variance = temp_pca$pca_obj$sdev ^ 2) %>%
                             dplyr::mutate(explained_variance = explained_variance
                                    / sum(explained_variance)) %>%
                             tibble::rownames_to_column(var = "component") %>%
                             dplyr::slice(1:n_comp)

                           return(temp_pca_share)


                         },.id = "partition")


  return(pca_expained_var_df)

}


