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


align_pca = function(pca_obj, var_name,
                     positive_direction = TRUE) {
  if (length(var_name) > 1) {
    sign_vec = sapply(as.data.frame(pca_obj$x),
                      function(temp_col) {
                        return(sign(
                          cor(x = temp_col, y = var_name,
                              use = "pairwise.complete.obs")
                        ))

                      })


  } else if (grepl("[0-9]", var_name)) {
    sign_vec = sign(pca_obj[["rotation"]][var_name, ])

  } else {
    sign_vec = sign(pca_obj[["rotation"]][
      rownames(pca_obj[["rotation"]]) == var_name, ])

  }

  if (length(sign_vec) == 0) {
    message(paste0(
      "align_pca: ",
      "Aligning coefficient missing ",
      "for ",
      var_name
    ))

    return(pca_obj)

  }


  if (!positive_direction) {
    sign_vec = sign_vec * (-1)
  }

  pca_obj$x = sapply(1:ncol(pca_obj$x), function(temp_ind) {
    pca_obj$x[, temp_ind] * sign_vec[temp_ind]
  })

  pca_obj$rotation = sapply(1:ncol(pca_obj$rotation), function(temp_ind) {
    pca_obj$rotation[, temp_ind] * sign_vec[temp_ind]
  })

  return(pca_obj)





}


#' This function reduces dimension based on PCA method
#' The function takes a data matrix and returns the first n_comps
#' components of PCA transformation. If the data matrix has a
#' time index the result is aligned along the index
#'
#' @title PCA reduction
#'
#' @importFrom stats prcomp
#'
#'
#' @param df dataframe
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
#' @return a list with two elements : pca_obj - PCA object, time_index - dates vector
#'
pca_reduction = function(df,
                         center = TRUE,
                         scale = TRUE,
                         sign_align_params = NULL) {

  . = NULL

  # Identify time index

  time_index_var = stringr::str_subset(names(df), "[Dd]ate")

  if (length(time_index_var) != 1) {
    message("Could not identify time index")
  }

  # Extract PCA

  df = df %>%
    dplyr::filter(complete.cases(.))

  if (ncol(df) == 1) {
    return(list(pca_obj = NULL, time_index = df[, time_index_var]))
  }

  temp_pca = df %>%
    dplyr::select(-dplyr::all_of(time_index_var)) %>%
    prcomp(center = center, scale. = scale)


  # Align PCA, if length == 2 then the second parameter is
  # the direction (boolean indicator).

  if (!is.null(sign_align_params)) {
    if (length(sign_align_params) == 2) {
      temp_pca = align_pca(
        pca_obj = temp_pca,
        var_name = sign_align_params[[1]],
        positive_direction = sign_align_params[[2]]
      )
    } else {
      temp_pca = align_pca(pca_obj = temp_pca,
                           var_name = sign_align_params[[1]])

    }



  }


  return(list(pca_obj = temp_pca, time_index = df[, time_index_var]))

}



#' This function purrr::maps pca reduction over partitions list
#'
#' @param multi_feature_partitions list of partitions
#'
#' @param vars_df raw data frame
#'
#' @param n_components number of components (default is 1)
#'
#' @param pca_align_list (optional) a named list of alignment parameters.
#' The name is the name of the targeted partition. The first element in
#' the list is the aligning (axis) variable, the value is either character
#' (variable's name) or numeric (variable's position index). The second element
#' (optional) is boolean indicator alignment direction (True means
#'  positive direction).
#'

map_pca_reduction = function(multi_feature_partitions,
                             vars_df,
                             n_components = 1,
                             pca_align_list = NULL) {

  reduction_objects_list = purrr::map2(
    names(multi_feature_partitions),
    multi_feature_partitions, function(temp_name, temp_part) {
      temp_df = vars_df %>%
        dplyr::select(dplyr::any_of(c(unlist(temp_part, use.names = FALSE), "date")))

      # Set alignment params

      if (temp_name %in% names(pca_align_list)) {

        temp_sign_align_params = pca_align_list[[temp_name]]

        } else {

          temp_sign_align_params = NULL

        }

      temp_pca = pca_reduction(df = temp_df,
                               sign_align_params = temp_sign_align_params)
      return(temp_pca)

      })

  names(reduction_objects_list) = names(multi_feature_partitions)

  xreg_df_multi = purrr::map(names(reduction_objects_list),function(temp_name) {
    date_vec = reduction_objects_list[[temp_name]]$time_index

    data_df = reduction_objects_list[[temp_name]]$pca_obj$x[, 1:n_components]

    if (is.null(data_df)) {

      data_df = matrix(nrow = length(date_vec),
                       ncol = n_components)
      }

    temp_df = cbind.data.frame(date_vec, data_df)

    if (n_components > 1) {
      names(temp_df) = c("date",
                         paste(rep(temp_name, n_components),
                               seq(1, n_components), sep = "_"))
      }

    else {

      names(temp_df) = c("date", temp_name)

      }

    return(temp_df)

                      }) %>%
    purrr::reduce(dplyr::full_join, by = "date")

  return_list = list()

  return_list$xreg_df_multi = xreg_df_multi

  return_list$reduction_objects_list = reduction_objects_list

  return(return_list)


}

#' @title PLS reduction
#'
#' @details This function reduces dimension based on PLS method
#' The function takes a data matrix and returns the first n_comps
#' components of PLS transformation. If the data matrix has a
#' time index the result is aligned along the index
#'
#' @param df dataframe
#'
#' @param target_var_name string that specifies outcome feature
#'
#' @param center boolean indicator
#'
#' @param scale boolean indicator
#'
#'
#' @return a list with two elements : pls_obj - PLS object,
#'  time_index - dates vector
#'
#' @importFrom  pls plsr
#'
#' @importFrom stringr stringr::str_remove_all
#'
pls_reduction = function(df,
                         target_var_name,
                         center = TRUE,
                         scale = TRUE) {
  . = NULL

  # Identify time index

  time_index_var = stringr::str_subset(names(df), "[Dd]ate")

  if (length(time_index_var) != 1) {
    message("Could not identify time index")
  }

  # Identify predictors names

  xvars_names = names(df) %>%
    stringr::str_remove_all(target_var_name) %>%
    stringr::str_remove_all(time_index_var)

  xvars_names = xvars_names[sapply(xvars_names,
                                   function(temp) {
                                     nchar(temp) > 0
                                   })]

  # Extract PLS

  df = df %>%
    dplyr::filter(complete.cases(.))

  pls_form = formula(paste(target_var_name, "~",
                           paste(xvars_names, collapse = "+")))

  temp_pls = df %>%
    dplyr::select(-dplyr::all_of(time_index_var)) %>%
    pls::plsr(
      formula = pls_form,
      validation = "none",
      scale = scale,
      center = center,
      data = .
    )

  return(list(pls_obj = temp_pls, time_index = df[, time_index_var]))

}



#' @title Map PLS reduction over partitions list
#'
#' @param multi_feature_partitions list of partitions
#'
#' @param vars_df raw data frame
#'
#' @param n_components number of components (default is 1)
#'
#' @param target_var_name string that specifies outcome feature
#'

map_pls_reduction = function(multi_feature_partitions,
                             vars_df,
                             target_var_name,
                             n_components = 1) {
  if (is.null(names(multi_feature_partitions))) {
    stop("The partitions must be a named list")
  }

  reduction_objects_list = purrr::map2(names(multi_feature_partitions),
                                multi_feature_partitions,
                                function(temp_name, temp_part) {
                                  temp_df = vars_df %>%
                                    dplyr::select(dplyr::any_of(c(
                                      unlist(temp_part), "date", target_var_name
                                    )))
                                  temp_pls = pls_reduction(df = temp_df,
                                                           target_var_name = target_var_name)
                                  return(temp_pls)

                                })

  names(reduction_objects_list) = names(multi_feature_partitions)

  xreg_df_multi = purrr::map(names(reduction_objects_list),
                      function(temp_name) {
                        date_vec = reduction_objects_list[[temp_name]]$time_index

                        data_df = reduction_objects_list[[
                          temp_name]]$pls_obj$scores[, 1:n_components]

                        temp_df = cbind.data.frame(date_vec, data_df)

                        if (n_components > 1) {
                          names(temp_df) = c("date",
                                             paste(rep(temp_name, n_components),
                                                   seq(1, n_components), sep = "_"))

                        } else {
                          names(temp_df) = c("date", temp_name)

                        }


                        return(temp_df)

                      }) %>%
    purrr::reduce(dplyr::full_join, by = "date")

  return_list = list()

  return_list$xreg_df_multi = xreg_df_multi

  return_list$reduction_objects_list = reduction_objects_list

  return(return_list)





}


#' This function preprocess data by reducing dimension and returns regression dataset
#'
#' @title Data dimension reduction
#'
#' @param vars_df a dataframe with all variables
#'
#' @param target_var_name string that specifies outcome feature
#'
#' @param partition_list a list of partitions for dimension reduction.
#' For elements in partition that contain only one variable the variable returns "as is".
#'
#' @param n_components number of components that should be returned
#'
#' @param preprocess_method (optional) string that specifies preprocess method
#' (default is pca)
#'
#' @param pca_align_list (optional) a named list of alignment parameters.
#' The name is the name of the targeted partition. The first element in
#' the list is the aligning (axis) variable, the value is either character
#' (variable's name) or numeric (variable's position index). The second element
#' (optional) is boolean indicator alignment direction (True means
#'  positive direction).
#'
#' @param return_objects_list boolean indicator that specifies whether a list
#' with object containing information (such as loadings) should be returned
#'
#' @return a list where first element is regression data (named xreg) and second
#' (optional) element is the pca obj list

reduce_data_dimension = function(vars_df,
                                 partition_list,
                                 target_var_name = NULL,
                                 n_components = 1,
                                 pca_align_list = NULL,
                                 preprocess_method = "pca",
                                 return_objects_list = FALSE) {
  # Validation

  if (is.null(partition_list)) {

    stop("The partition_list is NULL",call. = FALSE)

  }

  if (is.null(target_var_name) & preprocess_method == "pls") {

        stop(paste0("Target variable for PLS dimesion",
                    " reduction method is missing"),
         call. = FALSE)



  }

  if(!preprocess_method %in% c("pca", "pls")){

  stop("preprocess_method must be one of \"pca\" or \"pls\" ",
       call. = FALSE)


  }

  number_of_na = suppressWarnings(
    vars_df %>%
      dplyr::select(unlist(partition_list, use.names = FALSE)) %>%
      dplyr::mutate(across(everything(),as.numeric)) %>%
      is.na.data.frame() %>%
      sum()
    )

  if(number_of_na > 0){

    stop(paste0("vars df has missing values, dimesion reduction failed"),
         call. = FALSE)


  }

  return_list = list()

  one_feature_partitions = partition_list[sapply(partition_list, length) == 1]

  multi_feature_partitions = partition_list[sapply(partition_list, length) > 1]


  # Check for one variable partitions

  if (length(one_feature_partitions) > 0) {
    xreg_df_one = vars_df %>%
      dplyr::select(date, dplyr::any_of(unlist(one_feature_partitions, use.names = FALSE)))

  }


  # Reduce multi variable partitions

  if (length(multi_feature_partitions) > 0) {






    if (preprocess_method == "pca") {

      multi_part_return_list = map_pca_reduction(
        multi_feature_partitions = multi_feature_partitions,
        vars_df = vars_df,
        n_components = n_components,
        pca_align_list = pca_align_list
      )

    }


    if (preprocess_method == "pls") {
      multi_part_return_list = map_pls_reduction(
        multi_feature_partitions = multi_feature_partitions,
        vars_df = vars_df,
        target_var_name = target_var_name,
        n_components = n_components
      )


    }


    if (return_objects_list) {
      return_list$objects_list =
        multi_part_return_list$reduction_objects_list

    }


  }


  # Return xreg df and reduction objects (optional)


  if (length(one_feature_partitions) > 0 &
      length(multi_feature_partitions) > 0) {
    return_list$xreg_df = dplyr::inner_join(xreg_df_one,
                                     multi_part_return_list$xreg_df_multi,
                                     by = "date")

  }

  else if (length(multi_feature_partitions) > 0) {
    return_list$xreg_df = multi_part_return_list$xreg_df_multi


  }

  else {
    return_list$xreg_df = xreg_df_one

  }



  return(return_list)

}
