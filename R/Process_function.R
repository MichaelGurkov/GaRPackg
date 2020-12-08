
#' This function identifies consequitive NA sequences
#' at the beginning or end of data vector. This is an auxilary function
#' for interpolate
#'
#' @param data_vec
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
#' @param data_vec
#'
#' @importFrom zoo as.yearqtr as.yearmon
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
#'@import dplyr

chain_index = function(df, preprocess_method = "PCA", ...){

  date_varname = grep("[Dd]ate", names(df), value = TRUE)

  # Get list range

  time_indices_list = get_time_indices_list(df)

  # Get reduced diff series

  reduced_list = lapply(time_indices_list,function(temp_ind){

    temp_df = df %>%
      filter(!!sym(date_varname) %in% temp_ind) %>%
      select_if(~sum(is.na(.)) == 0)

    temp_agg_series = temp_df %>%
      pca_reduction(...) %>%
      mutate(PCA = scale(PCA))

    # debugging
    # if(sum(is.na(temp_agg_series$PCA)) > 0){browser()}


    temp_diff_series = temp_agg_series %>%
      mutate(PCA = PCA - lead(PCA)) %>%
      slice(-nrow(.))

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
    mutate(PCA = cumsum(PCA)) %>%
    arrange(date)

  return(chain_df)

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
      mutate_at(vars(-date_varname),
                .funs = list(~(dplyr::lead(., horizon) / .) ^ (1/horizon) - 1))

  } else{

    ret_df = df %>%
      mutate_at(vars(-date_varname),
                .funs = list(~(. / dplyr::lag(., horizon)) ^ (1/horizon) - 1))


  }


  ret_df = ret_df %>%
    mutate_at(vars(-date_varname), .funs = list(~(( 1 + .) ^ freq) - 1))

  return(ret_df)

}


#'This function creates a data set for quantile regression
#'
#'@details This functions applies preprocessing by reducing dimension of the data
#'(currently by PCA or PLS).
#'
#'@importFrom stats complete.cases
#'
#' @param partition_list a list of partitions for dimension reduction.
#' For elements in partition that contain only one variable the variable returns "as is".
#'
#' @param vars_df data frame with input variables
#'
#' @param target_var_name string that specifies outcome feature
#'
#' @param horizon_list list of forecast horizon
#'
#' @param partitions_list list of partition names
#'
#' @param preprocess_method string a method that aggregates the data to partitions
#'
#' @param return_objects_list boolean indicator that returns PCA objects.
#'
#'
make_quant_reg_df = function(vars_df,
                             target_var_name,
                             horizon_list,
                             preprocess_method = "inner_join_pca",
                             partitions_list = NULL,
                             pca.align.list = NULL,
                             return_objects_list = FALSE
                             ){


  if(preprocess_method == "asis"){

    vars_names = setdiff(names(vars_df), c(target_var_name, "date"))

    reg_df = vars_df %>%
       add_leads_to_target_var(target_var_name = target_var_name,
                              leads_vector = unlist(horizon_list)) %>%
      rename_at(vars(-c(target_var_name, "date")), ~paste0(.,"_xreg"))

    return(reg_df)


  }



  if(is.null(partitions_list)){

    reg_df = vars_df %>%
      select(date, all_of(target_var_name)) %>%
      filter(complete.cases(.)) %>%
      add_leads_to_target_var(target_var_name = target_var_name,
                              leads_vector = unlist(horizon_list))

    return(reg_df)


  }


    preproc_df_list = reduce_data_dimension(
      vars_df = vars_df,
      pca_align_list = pca.align.list,
      partitions_list = partitions_list,
      preprocess_method = preprocess_method,
      target_var_name = target_var_name,
      return_objects_list = return_objects_list
      )


    # Add lead values of target var

    reg_df = vars_df %>%
      select(date, all_of(target_var_name)) %>%
      inner_join(
        preproc_df_list$xreg_df %>%
          rename_at(vars(-date),~paste0(.,"_xreg")),
                 by = c("date" = "date")) %>%
      filter(complete.cases(.)) %>%
      add_leads_to_target_var(target_var_name = target_var_name,
                              leads_vector = unlist(horizon_list))




  return_list = list()

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
#' This function fixes the crossing issue in quantile regression
#'
#' @import tidyr
#'
#' @param prediction_df
#'
fix_quantile_crossing = function(prediction_df){

  prediction_df = prediction_df %>%
    group_by(horizon,date) %>%
    arrange(quantile) %>%
    mutate(across(contains("gar"),~sort(.))) %>%
    ungroup()


  return(prediction_df)




}


#' This helper function adds leads of target variable
#'
#' @param df
#'
#' @param target_var_name
#'
#' @param  leads_vector
#'
add_leads_to_target_var = function(df,
                                   target_var_name,leads_vector){


  for(temp_lead in unlist(leads_vector)){

    target_var_name_lead = paste(target_var_name,temp_lead,sep = "_")

    df = df %>%
      mutate(!!sym(target_var_name_lead) := lead(!!sym(target_var_name),temp_lead))

  }


  return(df)




}

#' This helper function calculates YoY rates of return
#'
#' @param variable_vec
#'
#' @import slider
#'
calculate_YoY_returns = function(variable_vec){

  yoy_vec = slide_dbl(.x = variable_vec,
                         .f = ~.[5]/.[1]-1,
                         .before = 4,
                         .complete = TRUE)

  return(yoy_vec)

}


#' This helper function calculates 4
#'
#' @param variable_vec
#'
#' @import slider
#'
calculate_four_quarters_ma = function(variable_vec){

  ma_vec = slide_dbl(.x = variable_vec,
                      .f = mean,
                      .before = 3,
                      .complete = TRUE)

  return(ma_vec)

}


#' This function returns a data frame with predicted values
#'
#' @title Make prediction df
#'
#' @details The default is in sample prediction (fitted values),
#' otherwise predict according to supplied xreg data
#'
#' @param gar_model
#'
#' @param xreg_df xreg data
#'
make_prediction_df = function(gar_model, xreg_df){

  prediction_df = map2_dfr(gar_model,names(gar_model),
           function(temp_mod, temp_name){

             temp_pred_df = xreg_df %>%
               select(date) %>%
               cbind(predict(temp_mod, xreg_df)) %>%
               pivot_longer(-date,
                            names_to = "Quantile",
                            values_to = "gar_fitted") %>%
               mutate(Quantile = str_remove_all(Quantile,"tau= ")) %>%
               mutate(Horizon = temp_name)



  }) %>%
    fix_quantile_crossing() %>%
    select(date,Horizon,Quantile,gar_fitted)

  return(prediction_df)



}
