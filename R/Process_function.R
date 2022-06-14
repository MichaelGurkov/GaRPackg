
#' This function identifies consequitive NA sequences
#' at the beginning or end of data vector. This is an auxilary function
#' for interpolate
#'
#' @param data_vec data series
#'
#'
identify_endpoints_NA = function(data_vec){

  na_ind = which(is.na(data_vec))

  if(length(na_ind) == 0 | length(na_ind) == length(data_vec)){return(NULL)}

  na_list = list()

  # Check for start NA

  if(min(na_ind) == 1){

    end_start_seq_ind = suppressWarnings(min(which(diff(na_ind) > 1)))

    if(is.finite(end_start_seq_ind)){

      na_list$start_indices = na_ind[1:end_start_seq_ind]

    } else

    {na_list$start_indices = na_ind}




  }

  # Check for end NA

  if(max(na_ind) == length(data_vec)){

    rev_na_ind = rev(na_ind)

    start_end_seq_ind = suppressWarnings(length(na_ind) -
      min(which(diff(rev_na_ind) < -1)) + 1)

    if(is.finite(start_end_seq_ind)){

      na_list$end_indices = na_ind[start_end_seq_ind:length(na_ind)]

    } else

      {na_list$end_indices = na_ind}


  }


  return(na_list)

}


#' This function interpolates forward by using linear interpolation
#'
#' @param data_vec numeric vector
#'
#' @param direction direction for interpolation (default is "forward")
#'
#' @importFrom zoo as.yearqtr as.yearmon na.approx
#'
#'
interpolate = function(data_vec, direction = "forward"){

  na_ind = which(is.na(data_vec))

  #Check for missing values, return the data if there are no NA

  if(length(na_ind) == 0){return(data_vec)}

  na_endpoints_list = identify_endpoints_NA(data_vec)

  clean_data = na.approx(data_vec)

  head_len = suppressWarnings(max(na_endpoints_list$start_indices) -
                                min(na_endpoints_list$start_indices) + 1)

  if(is.infinite(head_len)){head_len = 0}

  tail_len = suppressWarnings(max(na_endpoints_list$end_indices) -
                                min(na_endpoints_list$end_indices) + 1)

  if(is.infinite(tail_len)){tail_len = 0}


  if(direction == "forward"){

    final_point = clean_data[length(clean_data)]

    out_vec = c(rep(NA,head_len),clean_data,rep(final_point, tail_len))

  } else if (direction == "backward"){

    start_point =  clean_data[1]

    out_vec = c(rep(start_point,head_len),clean_data,rep(NA, tail_len))

  } else if (direction == "both"){

    start_point =  clean_data[1]

    final_point = clean_data[length(clean_data)]

    out_vec = c(rep(start_point,head_len),clean_data,
                rep(final_point, tail_len))
  }

  return(out_vec)

}


#' This function gets the time index for non missing
#' data for each variable in the data set and returns a list
#' of unique time indices
#'
#' @title Get list of unique time indices of non missing data
#'
#' @param df dataframe
#'
get_time_indices_list = function(df){

  time_index_name = grep("[Dd]ate", names(df), value = TRUE)

  if(!length(time_index_name) == 1){
    stop("Couldn't identify time index name")
  }


  indices_list = lapply(names(df)[!names(df) == time_index_name],
                      function(temp_name){
                        df[[time_index_name]][!is.na(df[[temp_name]])]
                        })

  indices_list = indices_list[!duplicated(indices_list)]

  return(indices_list)


}


#' This function creates a chained index partition
#'
#' @import dplyr
#'
#' @importFrom rlang .data
#'
#' @param df data frame with pca data
#'
#' @param preprocess_method method for dimension reduction (default is PCA)
#'
#' @param ... external optional arguments

chain_index = function(df, preprocess_method = "pca", ...){

  . = NULL

  date_varname = grep("[Dd]ate", names(df), value = TRUE)

  # Get list range

  time_indices_list = get_time_indices_list(df)

  # Get reduced diff series

  reduced_list = lapply(time_indices_list,function(temp_ind){

    temp_df = df %>%
      dplyr::filter(!!sym(date_varname) %in% temp_ind) %>%
      dplyr::select_if(~sum(is.na(.)) == 0)

    temp_agg_series = temp_df %>%
      pca_reduction(...) %>%
      dplyr::mutate(PCA = scale(.data$PCA))

    # debugging
    # if(sum(is.na(temp_agg_series$PCA)) > 0){browser()}


    temp_diff_series = temp_agg_series %>%
      dplyr::mutate(PCA = .data$PCA - lead(.data$PCA)) %>%
      dplyr::slice(-nrow(.))

    return(list(agg_series = temp_diff_series,
                num_vars = ncol(temp_df)))

                        })

  num_vars_vec = sapply(reduced_list,
                        function(temp_list){temp_list$num_vars})

  reduced_list = reduced_list[order(num_vars_vec,
                                    decreasing = TRUE)]

  # Append incremental series

  diff_df = reduced_list[[1]]$agg_series

  if(length(reduced_list) > 1){

    for (i in 2:length(reduced_list)){

      target_df = reduced_list[[i]]$agg_series

      temp_date_varname = grep("[Dd]ate", names(target_df),
                               value = TRUE)

      target_ind = (!target_df[[temp_date_varname]] %in%
                      diff_df[[temp_date_varname]])

      diff_df = rbind.data.frame(diff_df, target_df[target_ind,])


    }



  }

  diff_df = diff_df %>%
    arrange(date)

  # Return cumsum result

  chain_df = diff_df %>%
    arrange(desc(date)) %>%
    dplyr::mutate(PCA = cumsum(.data$PCA)) %>%
    arrange(date)

  return(chain_df)

}



#'This function creates a data set for quantile regression
#'
#'@details This functions applies preprocessing by reducing dimension of the data
#'(currently by PCA or PLS).
#'
#'@importFrom stats complete.cases
#'
#'
#' @param vars_df data frame with input variables
#'
#' @param target_var_name string that specifies outcome feature
#'
#' @param horizon_list list of forecast horizon
#'
#' @param partitions_list a list of partitions for dimension reduction.
#' For elements in partition that contain only one variable the variable returns "as is".
#'
#' @param pca.align.list named list where the names are partition names and the
#' first element is the aligning variable name. The second (optional) element
#' is the alignment direction (default is "positive")
#'
#' @param preprocess_method string a method that aggregates the data to partitions
#'
#' @param return_objects_list boolean indicator that returns PCA objects.
#'
#' @return regression data frame
#'
#'
make_quant_reg_df = function(vars_df,
                             target_var_name,
                             horizon_list,
                             preprocess_method = "pca",
                             partitions_list = NULL,
                             pca.align.list = NULL,
                             return_objects_list = FALSE
                             ){

  . = NULL

  return_list = list()


  if(preprocess_method == "asis"){

    vars_names = setdiff(names(vars_df), c(target_var_name, "date"))

    reg_df = vars_df %>%
       add_leads_to_target_var(target_var_name = target_var_name,
                              leads_vector = unlist(horizon_list)) %>%
      rename_at(vars(-c(target_var_name, "date")), ~paste0(.,"_xreg"))

    return_list$reg_df = reg_df

    return(return_list)


  }


  if(is.null(partitions_list)){

    reg_df = vars_df %>%
      dplyr::select(date, dplyr::all_of(target_var_name)) %>%
      dplyr::filter(complete.cases(.)) %>%
      add_leads_to_target_var(target_var_name = target_var_name,
                              leads_vector = unlist(horizon_list))

    return_list$reg_df = reg_df

    return(return_list)


  }


  if(!length(setdiff(unlist(partitions_list, use.names = FALSE),
                    names(vars_df))) == 0){

    stop(paste("Can't make quant reg df. The following variables are missing :",
                  paste0(setdiff(unlist(partitions_list, use.names = FALSE),
                                 names(vars_df)), collapse = ",")),
         call. = FALSE)



  }



    preproc_df_list = reduce_data_dimension(
      vars_df = vars_df,
      pca_align_list = pca.align.list,
      partition_list = partitions_list,
      preprocess_method = preprocess_method,
      target_var_name = target_var_name,
      return_objects_list = return_objects_list
      )


    # Add lead values of target var

    reg_df = vars_df %>%
      dplyr::select(date, dplyr::all_of(target_var_name)) %>%
      dplyr::inner_join(
        preproc_df_list$xreg_df %>%
          rename_at(vars(-date),~paste0(.,"_xreg")),
                 by = c("date" = "date")) %>%
      dplyr::filter(complete.cases(.)) %>%
      add_leads_to_target_var(target_var_name = target_var_name,
                              leads_vector = unlist(horizon_list))


    return_list$reg_df = reg_df


  if(return_objects_list & (!is.null(partitions_list))){
    return_list$pca_obj = preproc_df_list$objects_list}


  return(return_list)

}



#' @title Fill na with average of k previous observations
#'
#' @param data_vec vector of data
#'
#' @param k window size
#'

fill_na_average = function(data_vec, k = 4){

  na_ind = which(is.na(data_vec))

  fill_values = sapply(na_ind, function(temp_ind){

    start_ind = temp_ind - k

    end_ind = temp_ind - 1

    if(start_ind > 0 & end_ind > 0){

      return(mean(data_vec[start_ind:end_ind]))

    } else {

      return(NA)

    }

    })

  filled_data_vec = data_vec

  filled_data_vec[na_ind] = fill_values

  return(filled_data_vec)

}

#' @title Fixes quantile regression crossing
#'
#' @description  This function fixes the crossing issue in quantile
#'  regression by reordering the values according to quantiles order
#'
#' @import tidyr
#'
#' @importFrom rlang .data
#'
#' @param prediction_df data frame with prediction values. The structure is
#' date, horizon, quantile, forecast_values or fitted_values
#'
fix_quantile_crossing = function(prediction_df){

  prediction_df = prediction_df %>%
    dplyr::group_by(.data$horizon,.data$date) %>%
    dplyr::arrange(.data$quantile) %>%
    dplyr::mutate(dplyr::across(tidyselect::matches("^(fitted|forecast)_values$"),
                         ~sort(.))) %>%
    dplyr::ungroup()


  return(prediction_df)




}


#' @title  This helper function adds leads of target variable
#'
#' @importFrom rlang :=
#'
#' @param df raw data frame
#'
#' @param target_var_name regression depended variable
#'
#' @param leads_vector numeric vector with lead horizons
#'
add_leads_to_target_var = function(df,
                                   target_var_name,leads_vector){


  for(temp_lead in unlist(leads_vector)){

    target_var_name_lead = paste(target_var_name,temp_lead,sep = "_")

    df = df %>%
      dplyr::mutate(!!sym(target_var_name_lead) := lead(!!sym(target_var_name),temp_lead))

  }


  return(df)




}

#' @title Calculate year on year percent changes
#'
#' @description  This helper function calculates the percent change
#' between parallel periods in  two different years.
#'
#' @param variable_vec data series
#'
#' @param data_frequency string that specifies time frequency of the data.
#'
#' Available options are :
#' \itemize{
#'  \item {quarterly}{ (the default)}
#'  \item {monthly}
#' }
#'
#' @importFrom  slider slide_dbl
#'
calculate_yoy_changes = function(variable_vec,
                                    data_frequency = "quarterly"){

  if(data_frequency == "quarterly"){

    win_len = 4

  } else if(data_frequency == "monthly"){


    win_len = 12

  }


  percent_change_vec = slide_dbl(.x = variable_vec,
                         .f = ~.[win_len + 1]/.[1]-1,
                         .before = win_len,
                         .complete = TRUE)

  return(percent_change_vec)

}


#' This helper function calculates 4 points moving average
#'
#' @param variable_vec data series
#'
#' @importFrom  slider slide_dbl
#'
calculate_four_quarters_ma = function(variable_vec){

  ma_vec = slide_dbl(.x = variable_vec,
                      .f = mean,
                      .before = 3,
                      .complete = TRUE)

  return(ma_vec)

}

#' This function calculates the compound annualized growth rate
#'
#' @param df dataframe  - time indexed variable
#'
#' @param horizon the horizon of the change period
#'
#' @param freq time frequency of the data
#'
#' @param forward determines whether the calculation is forward
#' (meaning that at each time point the change is between lead
#'  and current point) or backward (at each time point the change
#'  is between current point and its lag) looking.
#'

calculate_CAGR = function(df, horizon, freq = 4, forward = TRUE){

  date_varname = grep("[Dd]ate",names(df),value = TRUE)

  if(!length(date_varname) == 1){
    stop("Couldn't identify time index variable")
  }


  if(forward){

    ret_df = df %>%
      dplyr::mutate_at(vars(-date_varname),
                .funs = list(~(dplyr::lead(., horizon) / .) ^ (1/horizon) - 1))

  } else{

    ret_df = df %>%
      dplyr::mutate_at(vars(-date_varname),
                .funs = list(~(. / dplyr::lag(., horizon)) ^ (1/horizon) - 1))


  }


  ret_df = ret_df %>%
    dplyr::mutate_at(vars(-date_varname), .funs = list(~(( 1 + .) ^ freq) - 1))

  return(ret_df)

}


#' @title Preprocess raw data
#'
#' @description This function performs preprocessing variable transformation
#'
#' @details The following transformations are supported
#' \itemize{
#'  \item{Year on Year percent change}
#'  \item{Differencing}
#'  \item{Annual moving average (the variables should be at quarterly frequency)}
#' }
#'
#' In case of Year on Year percent change the function identifies the time
#' frequency (by checking date class, either yearqtr or yearmon) and calculates
#' the percent change appropriately
#'
#' @param df raw data frame
#'
#' @param vars_to_yoy (optional) vector of variable names
#' for "Year on Year" transformation. Computes the percent change
#' between parallel periods in  two different years.
#'
#' @param vars_to_percent_changes (optional) vector of variable names
#' Computes the percent change between two sequential  periods.
#'
#' @param vars_to_diff (optional) vector of variable names for differencing transformation
#'
#' @param vars_to_4_ma (optional) vector of variable names for "Annual moving average"
#' transformation
#'
#' @param convert_to_percent_units logical default is FALSE should the relative change
#' variables be converted to percent units (multiply by 100).
#'
#' @export
preprocess_df = function(df,
                         vars_to_yoy = NULL,
                         vars_to_percent_changes = NULL,
                         vars_to_diff = NULL,
                         vars_to_4_ma = NULL,
                         convert_to_percent_units = FALSE) {

  if(!"date" %in% names(df)){

    stop("date variable is missing")

  } else if(class(df$date) == "yearqtr"){

    data_frequency = "quarterly"

  } else if(class(df$date) == "yearmon"){

    data_frequency = "monthly"

  } else {

    stop("The date variable must be yearqtr or yearmon class")

  }


  if(!is_null(vars_to_yoy)){

    if(!length(setdiff(vars_to_yoy,
                       names(df))) == 0){

      warning(paste("The following variables are missing :",
                    paste0(setdiff(vars_to_yoy,
                                   names(df)), collapse = ",")))



    }




    df = df %>%
      dplyr::mutate(across(
        dplyr::any_of(vars_to_yoy),
        list(yoy = ~calculate_yoy_changes(.,
                                          data_frequency = data_frequency))))



  }

  if(!is_null(vars_to_percent_changes)){

    if(!length(setdiff(vars_to_percent_changes,
                       names(df))) == 0){

      warning(paste("The following variables are missing :",
                    paste0(setdiff(vars_to_percent_changes,
                                   names(df)), collapse = ",")))



    }

    df = df %>%
      dplyr::mutate(across(dplyr::any_of(vars_to_percent_changes),
                    list(percent_change = ~ . / lag(., 1) - 1)))

  }

  if(!is_null(vars_to_diff)){

    if(!length(setdiff(vars_to_diff,
                       names(df))) == 0){

      warning(paste("The following difference variables are missing :",
                    paste0(setdiff(vars_to_diff,
                                   names(df)), collapse = ",")))



    }

    df = df %>%
      dplyr::mutate(across(dplyr::any_of(vars_to_diff),
                    list(diff = ~c(NA, diff(.)))))



  }

  if(!is_null(vars_to_4_ma)){

    if(!length(setdiff(vars_to_4_ma,
                       names(df))) == 0){

      warning(paste("The following moving average variables are missing :",
                    paste0(setdiff(vars_to_4_ma,
                                   names(df)), collapse = ",")))



    }

    df = df %>%
      dplyr::mutate(across(dplyr::any_of(vars_to_4_ma),
                    list(`4_ma` = ~calculate_four_quarters_ma(.))))



  }

  if(convert_to_percent_units){

    target_vars = c(paste0(vars_to_yoy,"_yoy"),
                    paste0(vars_to_percent_changes,"_percent_change"),
                    paste0(vars_to_4_ma,"_4_ma"))

    df = df %>%
      dplyr::mutate(across(dplyr::any_of(target_vars), ~ . * 100))

  }


  return(df)


}
