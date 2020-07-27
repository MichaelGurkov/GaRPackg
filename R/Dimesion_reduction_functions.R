#' Aligns the sign of PCA components
#'
#' @importFrom stats cor
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
#' @return PCA object with aligned PCA matrix
#'  (x component of the PCA object list). The loadings are left unchaged.


align.pca = function(pca_obj, var_name,  positive_direction = TRUE){


  if(length(var_name) > 1){

    sign_vec = sapply(as.data.frame(pca_obj$x), function(temp_col){

      return(sign(cor(x = temp_col, y = var_name,
                      use = "pairwise.complete.obs")))

    })


  } else if(grepl("[0-9]",var_name)){

    sign_vec = sign(pca_obj[["rotation"]][var_name,])

  } else {

    sign_vec = sign(pca_obj[["rotation"]][
      rownames(pca_obj[["rotation"]]) == var_name,])

  }

  if(length(sign_vec) == 0){

    message("align.pca: Aligning coefficient missing")

    return(pca_obj)

  }


  if(!positive_direction){sign_vec = sign_vec * (-1)}

  pca_obj$x = sapply(1:ncol(pca_obj$x),function(temp_ind){
    pca_obj$x[,temp_ind] * sign_vec[temp_ind]})

  pca_obj$rotation = sapply(1:ncol(pca_obj$rotation),function(temp_ind){
    pca_obj$rotation[,temp_ind] * sign_vec[temp_ind]})

  return(pca_obj)





}


#' This function reduces dimension based on PCA method
#' The function takes a data matrix and returns the first n_comps
#' components of PCA transformation. If the data matrix has a
#' time index the result is aligned along the index
#'
#' @title PCA reduction
#'
#'
#' @param df dataframe
#'
#' @param target_var_name string that specifies outcome feature
#'
#' @param center boolean indicator
#'
#' @param scale boolean indicator
#'
#' @param sign_align_params (optional) a list of alignment parameters.
#' The first element in the list is the aligning (axis) variable, the value is either
#' character (variable's name) or numeric (variable's position index). The second element
#' if(supplied) is boolean indicator alignment direction (True means positive direction).
#'
#' @return a list with two elements : pca_obj - PCA object, time_index - Dates vector
#'
pca_reduction = function(df,center = TRUE,
                         scale = TRUE,
                         sign_align_params = NULL){

  # Identify time index

  time_index_var = str_subset(names(df), "[Dd]ate")

  if(length(time_index_var) != 1){message("Could not identify time index")}

  # Extract PCA

  df = df %>%
    filter(complete.cases(.))

  temp_pca = df %>%
    select(-all_of(time_index_var)) %>%
    prcomp(center = center,scale. = scale)


  # Align PCA, if length == 2 then the second parameter is
  # the direction (boolean indicator).

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


  return(list(pca_obj = temp_pca, time_index = df[,time_index_var]))

}



#' This function maps pca reduction over partitions list
#'
#' @param multi_feature_partitions list of partitions
#'
#' @param vars_df
#'
#' @param n_components
#'
#' @param pca_align_list (optional) a list with alignment parameters. For each partition
#'  the aligning (axis) variable is specified together with aligning direction (optional).
#'
#'

map_pca_reduction = function(multi_feature_partitions,
                             vars_df,
                             n_components,
                             pca_align_list = NULL){

  reduction_objects_list = map2(
    names(multi_feature_partitions),
    multi_feature_partitions,
    function(temp_name, temp_part){

      temp_df = vars_df %>%
        select(any_of(c(unlist(temp_part),"Date")))

      # Set alignment params

      if(temp_name %in% names(pca_align_list)){

        temp_sign_align_params = pca_align_list[[temp_name]]

      } else {

        temp_sign_align_params = NULL

      }

      temp_pca = pca_reduction(
        df = temp_df,
        sign_align_params = temp_sign_align_params
      )


      return(temp_pca)

    })

  names(reduction_objects_list) = names(multi_feature_partitions)

  xreg_df_multi = map(names(reduction_objects_list),
                      function(temp_name){

                        Date_vec = reduction_objects_list[[temp_name]]$time_index

                        data_df = reduction_objects_list[[temp_name]]$pca_obj$x[
                          ,1:n_components]

                        temp_df = cbind.data.frame(Date_vec, data_df)

                        if(n_components > 1){

                          names(temp_df) = c(
                            "Date",
                            paste(rep(temp_name, n_components),
                                  seq(1,n_components), sep = "_"))

                        } else {


                          names(temp_df) = c("Date", temp_name)

                        }


                        return(temp_df)

                      }) %>%
    reduce(full_join, by = "Date")

  return_list = list()

  return_list$xreg_df_multi = xreg_df_multi

  return_list$reduction_objects_list = reduction_objects_list

  return(return_list)





}

#' This function reduces dimension based on PLS method
#' The function takes a data matrix and returns the first n_comps
#' components of PLS transformation. If the data matrix has a
#' time index the result is aligned along the index
#'
#' @title PLS reduction
#'
#'
#' @param df dataframe
#'
#' @param target_var_name string that specifies outcome feature
#'
#' @param center boolean indicator
#'
#' @param scale boolean indicator
#'
#' @param sign_align_params (optional) a list of alignment parameters.
#' The first element in the list is the aligning (axis) variable,
#' the value is either character (variable's name) or numeric
#' (variable's position index). The second element if(supplied) is
#' boolean indicator alignment direction
#' (True means positive direction).
#'
#' @return a list with two elements : pls_obj - PLS object,
#'  time_index - Dates vector
#'
#' @importFrom  pls plsr
#'
pls_reduction = function(df,target_var_name,center = TRUE,
                         scale = TRUE){

  # Identify time index

  time_index_var = str_subset(names(df), "[Dd]ate")

  if(length(time_index_var) != 1){

    message("Could not identify time index")}

  # Identify predictors names

  xvars_names = names(df) %>%
    str_remove_all(target_var_name) %>%
    str_remove_all(time_index_var)

  xvars_names = xvars_names[sapply(xvars_names,
                                   function(temp){nchar(temp) > 0})]

  # Extract PLS

  df = df %>%
    filter(complete.cases(.))

  pls_form = formula(paste(target_var_name,"~",
                           paste(xvars_names, collapse = "+")))

  temp_pls = df %>%
    select(-all_of(time_index_var)) %>%
    plsr(
      formula = pls_form,
      validation = "none",
      scale = scale,
      center = center,
      data = .
    )

  return(list(pls_obj = temp_pls, time_index = df[,time_index_var]))

}



#' This function maps pls reduction over partitions list
#'
#' @param multi_feature_partitions list of partitions
#'
#' @param vars_df
#'
#' @param n_components
#'
#' @param target_var_name string that specifies outcome feature
#'

map_pls_reduction = function(multi_feature_partitions,
                             vars_df,
                             target_var_name,
                             n_components){

  if(is.null(names(multi_feature_partitions))){
    stop("The partitions must be a named list")}

  reduction_objects_list = map2(
    names(multi_feature_partitions),
    multi_feature_partitions,
    function(temp_name, temp_part){

      temp_df = vars_df %>%
        select(any_of(c(unlist(temp_part),"Date",target_var_name)))
      temp_pls = pls_reduction(
        df = temp_df,
        target_var_name = target_var_name
      )
      return(temp_pls)

    })

  names(reduction_objects_list) = names(multi_feature_partitions)

  xreg_df_multi = map(names(reduction_objects_list),
                      function(temp_name){
                        Date_vec = reduction_objects_list[[
                          temp_name]]$time_index

                        data_df = reduction_objects_list[[
                          temp_name]]$pls_obj$scores[,1:n_components]

                        temp_df = cbind.data.frame(
                          Date_vec, data_df
                        )

                        if(n_components > 1){

                          names(temp_df) = c(
                            "Date",
                            paste(rep(temp_name, n_components),
                                  seq(1,n_components), sep = "_"))

                        } else {


                          names(temp_df) = c("Date", temp_name)

                        }


                        return(temp_df)

                      }) %>%
    reduce(full_join, by = "Date")

  return_list = list()

  return_list$xreg_df_multi = xreg_df_multi

  return_list$reduction_objects_list = reduction_objects_list

  return(return_list)





}


#' This function preprocess data by reducing dimension and returns regression dataset
#'
#' @title Data dimesion reduction
#'
#' @param vars_df a dataframe with all variables
#'
#' @param target_var_name string that specifies outcome feature
#'
#' @param partition a list of partitions for dimension reduction
#'
#' @param n_components number of components that should be returned
#'
#' @param method (optional) string that specifies dimesion reduction method
#' (default is PCA)
#'
#' @param pca_align_list (optional) a list with alignment parameters. For each partition
#'  the aligning (axis) variable is specified together with aligning direction (optional).
#'
#' @param return_objects_list boolean indicator that specifies whether a list
#' with object containing information (such as loadings) should be returned
#'
#' @return a list where first element is regression data (named xreg) and second
#' (optional) element is the pca obj list

reduce_data_dimension = function(vars_df,
                                 partition,
                                 target_var_name = NULL,
                                 n_components = 1,
                                 pca_align_list = NULL,
                                 method = "inner_join_pca",
                                 return_objects_list = FALSE){

  # Validation

  if(is.null(partition)){

    warning("The partition is NULL")

    return(NULL)

  }

  if(is.null(target_var_name) & method == "pls"){

    message("Target variable is NULL")

    return(NULL)



  }

  return_list = list()

  one_feature_partitions = partition[sapply(partition, length) == 1]

  multi_feature_partitions = partition[sapply(partition, length) > 1]


  # Check for one variable partitions

  if(length(one_feature_partitions) > 0){

    xreg_df_one = vars_df %>%
      select(Date, unlist(one_feature_partitions)) %>%
      mutate(across(-Date,scale))

  }


  # Reduce multi variable partitions


  if(length(multi_feature_partitions) > 0){


    if(method == "inner_join_pca"){
      multi_part_return_list = map_pca_reduction(
        multi_feature_partitions = multi_feature_partitions,
        vars_df = vars_df,
        n_components = n_components,
        pca_align_list = pca_align_list
      )

    }


    if(method == "pls"){

      multi_part_return_list = map_pls_reduction(
        multi_feature_partitions = multi_feature_partitions,
        vars_df = vars_df,
        target_var_name = target_var_name,
        n_components = n_components
      )


    }


    if(return_objects_list){

      return_list$objects_list =
        multi_part_return_list$reduction_objects_list

    }


  }


  # Return xreg df and reduction objects (optional)


  if(length(one_feature_partitions) > 0 &
     length(multi_feature_partitions) > 0){

    return_list$xreg_df = inner_join(
      xreg_df_one,
      multi_part_return_list$xreg_df_multi,
      by = "Date"
    )

  }

  else if (length(multi_feature_partitions) > 0){

    return_list$xreg_df = multi_part_return_list$xreg_df_multi


  }

  else {


    return_list$xreg_df = xreg_df_one

  }



  return(return_list)

}
