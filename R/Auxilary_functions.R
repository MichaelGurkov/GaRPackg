#' @title Return combinations of partition elements
#'
#' @description  This function returns a data_frame with list-column
#' partition combs
#'
#' @param optional_vars_vec
#'
#' @param required_vars_vec
#'
#' @param partition_name
#'
#' @import purrr
#'
#' @import tibble
#'

get_partition_combs = function(partitions_list,
                               partition_name) {

  # if only required category present return as data frame

  if (all(length(names(partitions_list)) == 1 &
          names(partitions_list) == "required")) {

    temp_comb_df = partitions_list %>%
      enframe %>%
      mutate(value = map(value, function(temp_vec) {
        temp_list = list(temp_vec)

        names(temp_list) = partition_name

        return(temp_list)

      })) %>%
      rename(!!sym(partition_name) := value) %>%
      mutate(name = paste(partition_name, 1, sep = "-"))


    return(temp_comb_df)
  }

  # make all combinations of optional category --> comb_df

  comb_df = map(seq_along(partitions_list$optional),
                function(temp_ind) {
                  comb_list =  combn(partitions_list$optional,
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
      mutate(value = map(value, ~ c(., partitions_list$required)))
  }


  comb_df = comb_df %>%
    mutate(value = map(value, function(temp_vec) {
      temp_list = list(temp_vec)

      names(temp_list) = partition_name

      return(temp_list)

    }))

  comb_df = comb_df %>%
    rename(!!sym(partition_name) := value)

  return(comb_df)

}



#' This function extracts the coefficients from quantile regression
#'
#'
#' @param qreg_object quantile regression object
#'
#'
extract.qreg.coeff.table = function(qreg_obj) {
  coef_table = lapply(suppressWarnings(summary(qreg_obj)),
                      function(temp_list) {
                        temp_df = as.data.frame(temp_list$coefficients)

                        temp_df$tau = temp_list$tau

                        temp_df$Name = rownames(temp_df)

                        rownames(temp_df) = NULL

                        return(temp_df)

                      }) %>%
    bind_rows() %>%
    rename(
      Coeff = coefficients,
      Low = `lower bd`,
      High = `upper bd`,
      Tau = tau
    ) %>%
    mutate(Tau = as.character(Tau)) %>%
    mutate(Name = gsub("(Intercept)", "Intercept", Name, fixed = TRUE)) %>%
    mutate(Significant = factor(
      ifelse(High <= 0 | Low >= 0, "Significant",
             "Non Significant"),
      levels = c("Significant", "Non Significant")
    ))

  return(coef_table)

}


#' This function extracts the coefficients data frame from gar model
#'
#' @param gar_model
#'
#' @param partition_names optional which partitions to return
#'
#' @return coeffs_df

extract_coeffs_from_gar_model = function(gar_model,
                                         partition_names = NULL) {
  stopifnot("qreg_result" %in% names(gar_model))

  coeffs_df = map2_dfr(gar_model[["qreg_result"]],
                       names(gar_model[["qreg_result"]]),
                       function(temp_mod, temp_name) {
                         temp_df = temp_mod %>%
                           extract.qreg.coeff.table() %>%
                           mutate(Horizon = temp_name)


                       })

  if (!is.null(partition_names)) {
    coeffs_df = coeffs_df %>%
      filter(Name %in% partition_names)


  }

  return(coeffs_df)


}


#' @title Extract factor contribution from gar model
#'
#' This function extracts the factor contribution (coefficients
#'  multiplied by values) data frame from gar model
#'
#'  @import purrr
#'
#'  @import magrittr
#'
#' @param gar_model
#'
#' @param partition_names optional which partitions to return
#'
#' @return factor_contribution_df
#'
#' @export

extract_factor_contribution_from_gar_model = function(
  gar_model, quantile = "0.05") {
  stopifnot("qreg_result" %in% names(gar_model))

  data_mat = gar_model$reg_df %>%
    select(ends_with("_xreg")) %>%
    as.matrix()

  coeffs_df = gar_model %>%
    extract_coeffs_from_gar_model() %>%
    filter(!Name == "Intercept") %>%
    filter(Tau == quantile) %>%
    select(Coeff,Horizon, Name)



  factors_df = map_dfr(
    unique(coeffs_df$Horizon),function(temp_horizon){

      coef_vec = coeffs_df %>%
        filter(Horizon == temp_horizon) %>%
        select(Coeff) %>%
        unlist(use.names = FALSE)

      factors_df =  t(t(data_mat) * coef_vec)

      factors_df = factors_df %>%
        as.data.frame() %>%
        cbind(date = gar_model$reg_df$date) %>%
        mutate(horizon = temp_horizon)

      return(factors_df)

    })

  factors_df = factors_df %>%
    rename_all(~str_remove_all(.,"_xreg"))

  return(factors_df)

}

#' This function calculates quantile r2 score for prediction df
#'
#' @import dplyr
#'
#' @param pred_df
#'
#' @param actual_df
#'
#' @param benchmark_df
#'
#' @export
#'
collect_quantile_r2_score = function(pred_df, realized_df,
                                     benchmark_df) {
  prediction_df = pred_df %>%
    inner_join(benchmark_df,
               by = c("Quantile", "Horizon", "Forecast_Period")) %>%
    left_join(realized_df, by = "Forecast_Period")

  score_df = prediction_df %>%
    select(realized, prediction, benchmark, Quantile, Horizon) %>%
    filter(complete.cases(.)) %>%
    group_by(Quantile, Horizon) %>%
    summarise(
      score = quantile_r2_score(
        realized_values = realized,
        forecast_values = prediction,
        quantile = as.numeric(Quantile)[1],
        benchmark_values = benchmark
      ),
      .groups = "drop"
    )


  return(score_df)


}

#' @title Calculate skew and IQR measures
#'
#' @details This function takes gar object and
#' reg df and calculates skew and
#'
#' @param gar_model
#'
#' @export
#'
calculate_skew_and_iqr = function(gar_obj) {
  prediction_df = make_prediction_df(gar_model = gar_obj$qreg_result,
                                     xreg_df = gar_obj$reg_df)

  skew_df = prediction_df %>%
    pivot_wider(
      names_from = Quantile,
      values_from = gar_fitted,
      names_prefix = "q"
    ) %>%
    mutate(Skew = (0.5 * q0.75 + 0.5 * q0.25 - q0.50) /
             (0.5 * q0.75 - 0.5 * q0.25)) %>%
    mutate(IQR = q0.75 - q0.25)

  return(skew_df)


}



extract_coeffs_from_gar_model = function(gar_model,
                                         partition_names = NULL) {
  stopifnot("qreg_result" %in% names(gar_model))

  coeffs_df = map2_dfr(gar_model[["qreg_result"]],
                       names(gar_model[["qreg_result"]]),
                       function(temp_mod, temp_name) {
                         temp_df = temp_mod %>%
                           extract.qreg.coeff.table() %>%
                           mutate(Horizon = temp_name)


                       })

  if (!is.null(partition_names)) {
    coeffs_df = coeffs_df %>%
      filter(Name %in% partition_names)


  }

  return(coeffs_df)


}


#' @title Extract factor contribution from gar model
#'
#' This function extracts the factor contribution (coefficients
#'  multiplied by values) data frame from gar model
#'
#'  @importFrom  purrr map_dfr
#'
#'  @importFrom tibble rownames_to_column
#'
#'  @import magrittr
#'
#' @param gar_model
#'
#' @param partition_names optional which partitions to return
#'
#' @return factor_contribution_df
#'
#' @export

extract_pca_loadings_from_gar_model = function(gar_model) {

  if(!"pca_obj" %in% names(gar_model)){

    stop("The pca object is missing")
  }

  pca_loadings_df = map_dfr(gar_model$pca_obj, function(temp_pca) {
    temp_coeffs = temp_pca$pca_obj$rotation[, 1] %>%
      as.data.frame() %>%
      setNames("coeff") %>%
      rownames_to_column()


  },  .id = "partition")

  return(pca_loadings_df)

}


#' @description  This function compares two partitions
#'
#' @title compare two partitions
#'
#' @param source_partition
#'
#' @param target_partition
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
