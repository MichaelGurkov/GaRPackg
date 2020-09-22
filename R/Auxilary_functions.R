
#' This function returns a data_frame with list-column
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

get_partition_combs = function(partition_list,
                               partition_name){

  if(all(length(names(partition_list)) == 1 &
         names(partition_list) == "required")){
    temp_comb_df = partition_list %>%
      enframe %>%
      mutate(value = map(value, function(temp_vec){

                temp_list = list(temp_vec)


        names(temp_list) = partition_name

        return(temp_list)

  names(pca_obj) = names(partitions_list)[sapply(partitions_list,length) > 1]

      })) %>%
      rename(!!sym(partition_name) := value) %>%
      mutate(name = paste(partition_name,1, sep = "-"))

    return(temp_comb_df)
  }

  comb_df = map(seq_along(partition_list$optional),
                  function(temp_ind){

    comb_list =  combn(partition_list$optional,temp_ind,simplify = FALSE)

    temp_comb_df = comb_list %>%
      enframe %>%
      mutate(name = paste(partition_name,temp_ind, sep = "-"))

                  }) %>%
    bind_rows() %>%
    rbind(data.frame(name = paste0(partition_name,"-0"), value = ""))

  if("required" %in% names(partition_list)){

    comb_df = comb_df %>%
      mutate(value = map(value, ~ c(.,partition_list$required)))

  }


  comb_df = comb_df %>%
    mutate(value = map(value, function(temp_vec){

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
extract.qreg.coeff.table = function(qreg_obj){
  coef_table = lapply(suppressWarnings(summary(qreg_obj)),
                      function(temp_list){

                        temp_df = as.data.frame(temp_list$coefficients)

                        temp_df$tau = temp_list$tau

                        temp_df$Name = rownames(temp_df)

                        rownames(temp_df) = NULL

                        return(temp_df)

                      }) %>%
    bind_rows() %>%
    rename(Coeff = coefficients, Low = `lower bd`,
           High = `upper bd`, Tau = tau) %>%
    mutate(Tau = as.character(Tau)) %>%
    mutate(Name = gsub("(Intercept)","Intercept",Name, fixed = TRUE)) %>%
    mutate(Significant = factor(ifelse(High <= 0 | Low >= 0,"Significant",
                                       "Non Significant"),
                                levels = c("Significant","Non Significant")))

    return(coef_table)

}


#' This function extracts the coefficients data frame from gar model
#'
#' @param gar_model
#'
#' @param partition_names optional which partitions to return
#'
#' @return coeffs_df

extract.coeffs.from.gar.model = function(gar_model,
                                         partition_names = NULL){

  stopifnot("qreg_result" %in% names(gar_model))

  coeffs_df = map2_dfr(gar_model[["qreg_result"]],
                   names(gar_model[["qreg_result"]]),
                   function(temp_mod, temp_name){

                     temp_df = temp_mod %>%
                       extract.qreg.coeff.table() %>%
                       mutate(Horizon = temp_name)


                   })

  if(!is.null(partition_names)){

    coeffs_df = coeffs_df %>%
      filter(Name %in% partition_names)


  }

  return(coeffs_df)


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
collect_quantile_r2_score = function(pred_df,realized_df,
                                     benchmark_df){

  prediction_df = pred_df %>%
    inner_join(benchmark_df,
               by = c("Quantile","Horizon","Forecast_Period")) %>%
    left_join(realized_df, by = "Forecast_Period")

  score_df = prediction_df %>%
    select(realized, prediction, benchmark, Quantile, Horizon) %>%
    filter(complete.cases(.)) %>%
    group_by(Quantile, Horizon) %>%
    summarise(score = quantile.r2.score(
      realized_values = realized,
      forecast_values = prediction,
      quantile = as.numeric(Quantile)[1],
      benchmark_values = benchmark), .groups = "drop")


  return(score_df)


}

#' @title Calculate skew and IQR measures
#'
#' @details This function takes gar object and
#' reg df and calculates skew and
#'
#' @param gar_model
#'
calculate_skew_and_iqr = function(gar_obj){

  prediction_df = make_prediction_df(
    gar_model = gar_obj$qreg_result,
    xreg_df = gar_obj$reg_df)

  skew_df = prediction_df %>%
    pivot_wider(names_from = Quantile,
                values_from = GaR_fitted,
                names_prefix = "q") %>%
    mutate(Skew = (0.5 * q0.75 + 0.5 * q0.25 - q0.50) /
             (0.5 * q0.75 - 0.5 * q0.25)) %>%
    mutate(IQR = q0.75 - q0.25)

  return(skew_df)


}
