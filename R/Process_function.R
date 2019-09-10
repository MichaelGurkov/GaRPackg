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


#' This function reduces dimension based on pca method
#'
#'
#' @param data dataframe
#'
#' @import prcomp
#'
pca_reduction = function(data, time_index = TRUE,
                         scale = TRUE, n_comps = 1){

  if(time_index){

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

    return(data.frame(Date = as.character(time_index),
                      PCA = temp_pca$x[,1:n_comps]))

  } else {

    temp_pca = prcomp(data, scale. = scale)

    return(data.frame(PCA = temp_pca$x[,1:n_comps]))


  }




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
