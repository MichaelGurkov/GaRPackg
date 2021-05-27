#' @title Return combinations of partition elements
#'
#' @description  This function returns a data_frame with list-column
#' partition combs
#'
#' @import purrr
#'
#' @import tibble
#'
#' @importFrom rlang .data
#'
#' @importFrom rlang :=
#'
#' @importFrom utils combn
#'
#' @param partitions_list a named list where the names are
#' \itemize{
#'  \item{"optional"}{-Optional elements}
#'  \item{"required"}{-Required elements}
#'  }
#'
#' @param partition_name name of the partition for resulting df
#'
#'

get_partition_combs = function(partitions_list,
                               partition_name) {

  # if only required category present return as data frame

  if (all(length(names(partitions_list)) == 1 &
          names(partitions_list) == "required")) {

    temp_comb_df = partitions_list %>%
      enframe %>%
      mutate(value = map(.data$value, function(temp_vec) {
        temp_list = list(temp_vec)

        names(temp_list) = partition_name

        return(temp_list)

      })) %>%
      rename(!!sym(partition_name) := .data$value) %>%
      mutate(name = paste(partition_name, 1, sep = "-"))


    return(temp_comb_df)
  }

  # make all combinations of optional category --> comb_df

  comb_df = map(seq_along(partitions_list$optional),
                function(temp_ind) {
                  comb_list =  utils::combn(partitions_list$optional,
                                     temp_ind, simplify = FALSE)

                  temp_comb_df = comb_list %>%
                    enframe %>%
                    mutate(name = paste(partition_name, temp_ind, sep = "-"))
                }) %>%
    bind_rows() %>%
    rbind(data.frame(name = paste0(partition_name, "-0"), value = ""))

  # if required category present add to each combination in comb_df

  if ("required" %in% names(partitions_list)) {
    comb_df = comb_df %>%
      mutate(value = map(.data$value, ~ c(., partitions_list$required)))
  }


  comb_df = comb_df %>%
    mutate(value = map(.data$value, function(temp_vec) {
      temp_list = list(temp_vec)

      names(temp_list) = partition_name

      return(temp_list)

    }))

  comb_df = comb_df %>%
    rename(!!sym(partition_name) := .data$value)

  return(comb_df)

}



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
    bind_rows() %>%
    rename(
      coeff = .data$coefficients,
      low = .data$`lower bd`,
      high = .data$`upper bd`,
      quantile = .data$tau
    ) %>%
    mutate(quantile = as.character(.data$quantile)) %>%
    mutate(partition = gsub("(Intercept)", "Intercept", .data$partition, fixed = TRUE)) %>%
    mutate(significant = factor(
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
    map_dfr(extract_qreq_coeff_table,.id = "horizon") %>%
    relocate(.data$partition, .data$horizon, .data$quantile,
             .data$coeff, .data$low, .data$high, .data$significant) %>%
    mutate(partition = str_remove_all(.data$partition,"_xreg$"))

  if (!is.null(partition_names)) {
    coeffs_df = coeffs_df %>%
      filter(.data$partition %in% partition_names)


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
#' @param target_quantile filtering quantile (default 0.05)
#'
#' @return factor_contribution_df
#'
#' @export

extract_factor_contribution_from_gar_model = function(
  gar_model, target_quantile = "0.05") {
  stopifnot("qreg_result" %in% names(gar_model))

  data_mat = gar_model$reg_df %>%
    select(ends_with("_xreg")) %>%
    as.matrix()

  data_mat = cbind(rep(1,nrow(data_mat)),data_mat)


  coeffs_df = gar_model %>%
    extract_coeffs_from_gar_model() %>%
    filter(.data$quantile == target_quantile) %>%
    select(.data$coeff,.data$horizon, .data$partition)



  factors_df = map_dfr(
    unique(coeffs_df$horizon),function(temp_horizon){

      coef_vec = coeffs_df %>%
        filter(.data$horizon == temp_horizon) %>%
        select(.data$coeff) %>%
        unlist(use.names = FALSE)

      factors_df =  t(t(data_mat) * coef_vec)

      factors_df = factors_df %>%
        as.data.frame() %>%
        cbind(date = gar_model$reg_df$date) %>%
        mutate(horizon = temp_horizon)

      return(factors_df)

    })

  factors_df = factors_df %>%
    rename_all(~str_remove_all(.,"_xreg")) %>%
    rename(intercept = V1)

  return(factors_df)

}


#' @title Calculate skew and IQR measures
#'
#' @details This function takes gar object, extracts
#' predictions and calculates Skew and IQR measures
#'
#' @param gar_obj model object with run_GaR_analysis result
#'
#' @importFrom rlang .data
#'
#' @export
#'
calculate_skew_and_iqr = function(gar_obj) {
  prediction_df = make_prediction_df(gar_model = gar_obj$qreg_result,
                                     xreg_df = gar_obj$reg_df)

  skew_df = prediction_df %>%
    pivot_wider(
      names_from = .data$quantile,
      values_from = .data$gar_fitted,
      names_prefix = "q"
    ) %>%
    mutate(skew = (0.5 * .data$q0.75 + 0.5 * .data$q0.25 - .data$q0.50) /
             (0.5 * .data$q0.75 - 0.5 * .data$q0.25)) %>%
    mutate(iqr = .data$q0.75 - .data$q0.25)

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

  pca_loadings_df = map_dfr(gar_model$pca_obj, function(temp_pca) {
    temp_coeffs = temp_pca$pca_obj$rotation[, 1] %>%
      as.data.frame() %>%
      setNames("coeff") %>%
      rownames_to_column()


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

  pca_loadings_df = map2(gar_model$pca_obj, names(gar_model$pca_obj),
                             function(temp_pca, temp_name) {

    temp_pca_df = temp_pca$pca_obj$x[, 1:n_comp] %>%
      as.data.frame() %>%
      cbind(date = temp_pca$time_index) %>%
      relocate(date)

    if (n_comp > 1) {
      temp_pca_df = temp_pca_df %>%
        setNames(c("date",paste(temp_name, 1:n_comp, sep = "_")))

    } else {
      temp_pca_df = temp_pca_df %>%
        setNames(c("date",temp_name))

    }

    return(temp_pca_df)


  }) %>%
    reduce(full_join, by = "date")


  return(pca_loadings_df)

}


#' @description  This function compares two partitions
#'
#' @title compare two partitions
#'
#' @param source_partition benchmark partition
#'
#' @param target_partition compared partition
#'

is_partition_identical = function(source_partition, target_partition){

  names_diff = union(
    setdiff(names(source_partition),names(target_partition)),
    setdiff(names(target_partition),names(source_partition)))

  if(!length(names_diff) == 0){return(FALSE)}

  target_partition = target_partition[names(source_partition)]

  for(temp_name in names(source_partition)){

    if(!length(source_partition[[temp_name]]) ==
       length(target_partition[[temp_name]])){return(FALSE)}

    comp_diff = union(
      setdiff(source_partition[[temp_name]],target_partition[[temp_name]]),
      setdiff(target_partition[[temp_name]],source_partition[[temp_name]]))


    if(!length(comp_diff) == 0){return(FALSE)}


  }

  return(TRUE)


}


#' @title Smooth gar forecast with skew t distribution
#'
#' @description
#'
#' @param gar_forecast_df
#'
#' @param time_limit
#'
#' @export
#'
smooth_gar_forecast_with_t_skew = function(gar_forecast_df,
                                            time_limit = 10){

  if("forecast_values" %in% names(gar_forecast_df)){

    stop("forecast_values column is missing")

  }

  t_skew_fit_df = gar_forecast_df %>%
    rename(values = forecast_values) %>%
    mutate(across(c(quantile, values),as.numeric)) %>%
    group_by(date, horizon) %>%
    group_map(function(temp_df, temp_name){

      fit_params =  fit_t_skew(select(temp_df, c(
        "quantile", "values")),time_limit = time_limit)

      fit_params_df = tibble(temp_name,parameter = names(fit_params), value = fit_params)

      return(fit_params_df)

    }) %>%
    bind_rows()


  return(t_skew_fit_df)





}
