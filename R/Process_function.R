#' This function filters non NA observations and detrends the clean
#'  series using HP filter
#'
#'  @import mFilter
#'

detrend.HP = function(data, HP_gamma = NULL){

  non_na_ind = !is.na(data)

  HP_cycle = hpfilter(data[non_na_ind],freq = 400,type = "lambda")[["cycle"]]

  res = data

  res[non_na_ind] = HP_cycle

  return(res)

}


#' Aligns the sign of PCA components
#'
#' @description  This function aligns the PCA components with respect to chosen variable.
#'
#' @details PCA assigns arbitrary sign to coefficients (loadings).
#' This function aligns the PCA components according to given increase
#' direction of the chosen original variable. The default direction is positive
#'
#' @param pca_obj PCA object
#'
#' @param var_name string or numeric index. The name of the original
#' variable or it's column index in the data matrix
#'
#' @param positive_direction a boolean indicator of the direction of the
#' original variable influence
#'
#'  @return PCA object with aligned PCA matrix
#'  (x component of the PCA object list). The loadings are left unchaged.


align.pca = function(pca_obj, var_name,  positive_direction = TRUE){

  if(grepl("[0-9]",var_name)){

    sign_vec = sign(pca_obj[["rotation"]][var_name,])

  } else {

    sign_vec = sign(pca_obj[["rotation"]][
      rownames(pca_obj[["rotation"]]) == var_name,])

  }


  if(!positive_direction){sign_vec = sign_vec * (-1)}

  pca_obj$x = sapply(1:ncol(pca_obj$x),function(temp_ind){
    pca_obj$x[,temp_ind] * sign_vec[temp_ind]})

  return(pca_obj)





}


#' This function reduces dimension based on pca method
#' The function takes a data matrix and returns the first n_comps
#' components of PCA transformation. If the data matrix has a
#' time index the result is aligned along the index
#'
#' @title PCA reduction
#'
#'
#' @param data dataframe
#'
#' @param has_time_index boolean indicator of time index of the data
#'
#' @param n_comps (optional) number of PCA components to return
#'
#'
pca_reduction = function(data, has_time_index = TRUE,
                         scale = TRUE, n_comps = 1,
                         sign_align_params = NULL){

  if(has_time_index){

    date_varname = grep("^[Dd]ate$",names(data), value = TRUE)

    # Check for unique date variable

    if(length(date_varname) < 1){

      stop("Couldn't identify a time index variable")

    } else if(length(date_varname) > 1){

      stop(paste0("Identified multiple possible time index variables :",
                 paste0(date_varname, collapse = ",")))

    }

    time_index = data[,date_varname]

    pca_df = data[,!names(data) == date_varname]

    temp_pca = prcomp(pca_df, scale. = scale)

  } else {

    temp_pca = prcomp(data, scale. = scale)

  }

    # Align PCA, if length == 2 then the second parameter is the direction
    # boolean indicator.

    if(!is.null(sign_align_params)){

      if(length(sign_align_params) == 2){

        temp_pca = align.pca(pca_obj = temp_pca,
                             var_name = sign_align_params[[1]],
                             positive_direction = sign_align_params[[2]])
      } else {

        temp_pca = align.pca(pca_obj = temp_pca,
                             var_name = sign_align_params[[1]])



      }



    }


    if(has_time_index){

      temp_df = data.frame(Date = as.Date(time_index),
                           PCA = temp_pca$x[,1:n_comps])

    } else {


      temp_df = data.frame(PCA = temp_pca$x[,1:n_comps])

      }

    return(temp_df)


  }




#' This function identifies consequitive NA sequences
#' at the beginning or end of data vector. This is an auxilary function
#' for interpolate
#'
#' @param data_vec
#'
identify.endpoints.NA = function(data_vec){

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
#' @import zoo
#'
interpolate = function(data_vec, direction = "forward"){

  na_ind = which(is.na(data_vec))

  #Check for missing values, return the data if there are no NA

  if(length(na_ind) == 0){return(data_vec)}

  na_endpoints_list = identify.endpoints.NA(data_vec)

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
get.time.indices.list = function(df){

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

chain_index = function(df, method = "PCA", ...){

  Date_varname = grep("[Dd]ate", names(df), value = TRUE)

  # Get list range

  time_indices_list = get.time.indices.list(df)

  # Get reduced diff series

  reduced_list = lapply(time_indices_list,function(temp_ind){

    temp_df = df %>%
      filter(!!sym(Date_varname) %in% temp_ind) %>%
      select_if(~sum(is.na(.)) == 0)

    temp_agg_series = temp_df %>%
      pca_reduction(...) %>%
      mutate(PCA = scale(PCA))


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

  for (i in 2:length(reduced_list)){

    target_df = reduced_list[[i]]$agg_series

    temp_Date_varname = grep("[Dd]ate", names(target_df),
                             value = TRUE)

    target_ind = (!target_df[[temp_Date_varname]] %in%
                    diff_df[[temp_Date_varname]])

    diff_df = rbind.data.frame(diff_df, target_df[target_ind,])


  }

  diff_df = diff_df %>%
    arrange(Date)

  # Return cumsum result

  chain_df = diff_df %>%
    arrange(desc(Date)) %>%
    mutate(PCA = cumsum(PCA)) %>%
    arrange(Date)

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

calculate.CAGR = function(df, horizon, freq = 4, forward = TRUE){

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
#'@param partitions_df data frame with explanatory variables
#'
#'@param dep_var_df data frame with time indexed depended variable
#'
#'@param horizon forecasting horizon
#'
#'@param transform_to_rate parameter that defines whether to transform
#'the depended variable to growth rate
#'
#'
make.quant.reg.df = function(partitions_df, dep_var_df,
                             horizon, transform_to_rate = TRUE){

  if(transform_to_rate){

    dep_var_df = calculate.CAGR(dep_var_df,
                                horizon = horizon)

  }

  reg_df = dep_var_df %>%
    inner_join(partitions_df) %>%
    filter(complete.cases(.))

  return(reg_df)

}



#' This function extracts the coefficients from quantile regression
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
    mutate(Significant = ifelse(High <= 0 | Low >= 0,TRUE, FALSE))

  return(coef_table)





}
