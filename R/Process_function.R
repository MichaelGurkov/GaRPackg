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


#' This function interpolates forward by using linear interpolation
#'
#' @param data_vec
#'
#' @import zoo
#'
interpolate = function(data_vec, direction = "forward"){

  #Check for missing values, return the data if there are no NA

  if(!anyNA(data_vec)){return(data_vec)}

  # Interpolate

  if(direction == "forward"){

    clean_vec = na.approx(data_vec)

    vec_len = length(data_vec)

    clean_vec_len = length(clean_vec)

    out_vec = c(rep(NA, vec_len - clean_vec_len), clean_vec)

    return(out_vec)

  }




}
