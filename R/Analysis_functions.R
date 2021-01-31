#' @title Perform GaR analysis
#'
#' @description This is a convenient function that encapsulates the GaR analysis.
#' The stages of the analysis are :
#' \itemize{
#'   \item Perform PCA
#'   \item Run quantile regression
#' }
#'
#'
#' @importFrom stats formula
#'
#' @param partititions_list list of partitions
#'
#' @param vars_df data frame with input variables
#'
#' @param target_var_name string that specifies outcome feature
#'
#' @param horizon_list list of forecast horizon
#'
#' @param quantile_vec vector of required quantiles in quantile
#' regression (corresponds to tau argument in rq)
#'
#' @param preprocess_method string a method that aggregates the data to partitions
#'
#' @param run_ols_reg boolean indicator that adds an OLS regression
#'
#'
#' @param pca.align.list (Optional) A list that specifies the PCA aligning variable for
#' each partition and alignment direction (default is positive direction).
#' The first element in the list is the aligning (axis) variable, the
#' value is either character (variable's name) or numeric
#' (variable's position index). The second element if(supplied)
#' is a boolean indicator of alignment direction
#' (True means positive direction).
#' If no list is supplied then no alignment takes place
#'
#' @param return_objects_list boolean indicator that returns PCA objects.
#' Default is TRUE
#'
#' @return reg_df data frame with training set data
#'
#' @return qreg_result a quantile regression model object
#'
#' @return gar_fitted_df a data frame with fitted values
#'
#' @return pca_obj (optional) PCA object
#'
#' @export
#'
run_GaR_analysis = function(partitions_list, vars_df,
                            target_var_name,
                            horizon_list,
                            quantile_vec,
                            preprocess_method = "inner_join_pca",
                            run_ols_reg = FALSE,
                            pca.align.list = NULL,
                            return_objects_list = TRUE){


  reg_df_list = make_quant_reg_df(
    partitions_list = partitions_list,
    vars_df = vars_df,
    target_var_name = target_var_name,
    horizon_list = horizon_list,
    pca.align.list = pca.align.list,
    preprocess_method = preprocess_method,
    return_objects_list = return_objects_list
  )

  if(nrow(reg_df_list$reg_df) == 0){

   stop("the regression dataframe is empty")

  }



  qreg_result = run_quant_reg(
    reg_df = reg_df_list$reg_df,
    target_var_name = target_var_name,
    quantile_vec = quantile_vec,
    horizon_list = horizon_list
  )


  gar_fitted_df = map2_dfr(qreg_result, names(qreg_result),
                  function(temp_obj,temp_name){

                   temp_fitted_df =  temp_obj$fitted.values %>%
                     as.data.frame() %>%
                     setNames(quantile_vec) %>%
                     mutate(date = reg_df_list$reg_df$date[
                       1:nrow(temp_obj$model)]) %>%
                      pivot_longer(cols = -date,
                                   names_to = "quantile",
                                   values_to = "gar_fitted") %>%
                      mutate(horizon = temp_name)




                  }) %>%
    fix_quantile_crossing()



  # Run OLS regresion

  if(run_ols_reg){

    ols_result = lapply(horizon_list, function(temp_horizon){

      ols_reg = lm(formula = formula(paste0(dep_var,"~.")),
                   data = reg_df_list$reg_df %>%
                     select(-date) %>%
                     select(-contains(target_var_name), all_of(dep_var)))

      return(ols_reg)


    })

    names(ols_result) = horizon_list

  }


  # Check for objects and return list

  return_list = list()

  return_list$reg_df = reg_df_list$reg_df

  return_list$qreg_result = qreg_result

  return_list$gar_fitted_df = gar_fitted_df

  if(length(reg_df_list) == 2){
    return_list$pca_obj = reg_df_list$pca_obj}

  if(run_ols_reg){return_list$ols_result = preproc_df_list$ols_result}

  return(return_list)

}


#' This function runs quantile regression
#'
#' @import quantreg
#'
#' @param reg_df data for quantile regression
#'
#' @param target_var_name string
#'
#' @param quantile_vec
#'
#' @return list of quantile reg objects
#'
run_quant_reg = function(reg_df,
                         target_var_name,
                         quantile_vec,
                         horizon_list,
                         reg_type = "quantile",
                         ...){


  if(reg_type == "quantile"){

    qreg_result = map(horizon_list, function(temp_horizon){

      dep_var = paste(target_var_name, temp_horizon, sep = "_")

      qreg_list = rq(formula = formula(paste0(dep_var,"~.")),
                     tau = quantile_vec,
                     data = reg_df %>%
                       select(ends_with("_xreg"),
                              all_of(dep_var)))

      return(qreg_list)


    })

  }

  if(reg_type == "lasso"){

    qreg_result = map(horizon_list, function(temp_horizon){

      dep_var = paste(target_var_name, temp_horizon, sep = "_")

      y_mat = reg_df %>%
        select(dep_var) %>%
        filter(complete.cases(.))

      x_mat = reg_df %>%
        select(ends_with("_xreg"),
               all_of(dep_var)) %>%
        filter(complete.cases(.)) %>%
        select(ends_with("_xreg"))

      qreg_list = rq.fit.lasso(x = as.matrix(x_mat),
                               y = as.matrix(y_mat),
                               tau = quantile_vec,
                               ...)

      return(qreg_list)


    })

  }

  names(qreg_result) = horizon_list


  return(qreg_result)


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
                                            names_to = "quantile",
                                            values_to = "gar_fitted") %>%
                               mutate(quantile = str_remove_all(quantile,"tau= ")) %>%
                               mutate(horizon = temp_name)



                           }) %>%
    fix_quantile_crossing() %>%
    select(date,horizon,quantile,gar_fitted)

  return(prediction_df)



}
