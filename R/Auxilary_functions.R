#' @title Perform GaR analysis
#'
#' @description This is a convinent function that encupsulates the GaR analysis.
#' The stages of the analysis are :
#' \itemize{
#'   \item Perform PCA
#'   \item Run quantile regression
#' }
#'
#'
#' @importFrom stats formula
#'
#' @param partititions_list list of partitons
#'
#' @param vars_df data frame with input variables
#'
#' @param method string a method that aggregates the data to partitions
#'
#' @param horizon_list list of forecast horizon
#'
#' @param quantile_vec vector of required quantiles in quantile regression
#' (corresponds to tau argument in rq)
#'
#'
run.GaR.analysis = function(partitions_list, vars_df,horizon_list,
                            quantile_vec,method = "inner_join_PCA",
                            run_ols_reg = TRUE, rq_method = "br",
                            pca.align.list = NULL){

  # Make and align PCA

  pca_obj = lapply(names(partitions_list)[sapply(partitions_list,length) > 1],
         function(temp_name){

           temp_part = partitions_list[[temp_name]]

           available_names = names(vars_df)

           part_df = vars_df %>%
             select(unlist(temp_part)[unlist(temp_part) %in% available_names],
                    Date) %>%
             filter_all(all_vars(is.finite(.)))

           pca = part_df %>%
             select(-Date) %>%
             prcomp(.,center = TRUE,scale. = TRUE)

           # Align PCA

          if(temp_name %in% names(pca.align.list)){

            pca = do.call("align.pca",c(list(pca_obj = pca),
                                        pca.align.list[[temp_name]]))

          } else {

            pca = align.pca(pca,1)

          }



           ret_list = list(pca =  pca, date_index = part_df$Date)

            return(ret_list)

                           })

  names(pca_obj) = names(partitions_list)[sapply(partitions_list,length) > 1]

  # Make regression data frame

  reg_df = lapply(names(pca_obj),function(temp_name){
           temp_df = data.frame(Date = pca_obj[[temp_name]]$date_index,
                                PCA = pca_obj[[temp_name]]$pca$x[,1],
                                stringsAsFactors = FALSE)

           names(temp_df) = c("Date",temp_name)

           return(temp_df)

         }) %>%
    reduce(inner_join, by = "Date") %>%
    arrange(Date)%>%
    inner_join(vars_df %>%
                 select(Date, GDP), by = "Date")

  for(temp_horizon in unlist(horizon_list)){

    temp_var = paste("GDP_growth",temp_horizon,sep = "_")

    reg_df = reg_df %>%
      mutate(!!sym(temp_var) := lead(GDP,temp_horizon))

  }


  # Run quantile regression

  qreg_result = lapply(horizon_list, function(temp_horizon){

    dep_var = paste0("GDP_growth_", temp_horizon)

    qreg_list = rq(formula = formula(paste0(dep_var,"~.")),
                   tau = quantile_vec,
                   data = reg_df %>%
                     select(-Date) %>%
                     select(names(.)[!grepl("GDP",names(.))], dep_var),
                   method = rq_method)

    return(qreg_list)


  })

  names(qreg_result) = horizon_list


  # Run OLS regresion

  if(run_ols_reg){

    ols_result = lapply(horizon_list, function(temp_horizon){

      dep_var = paste0("GDP_growth_", temp_horizon)

      ols_reg = lm(formula = formula(paste0(dep_var,"~.")),
                     data = reg_df %>%
                       select(-Date) %>%
                       select(names(.)[!grepl("GDP",names(.))], dep_var))

      return(ols_reg)


    })

    names(ols_result) = horizon_list

    return(list(pca = pca_obj, reg_df = reg_df, quantile_reg = qreg_result,
                ols_reg = ols_result))


  } else {


    return(list(pca = pca_obj, reg_df = reg_df, quantile_reg = qreg_result))

  }




}


#' This is a convinience functions that plots the coefficients of
#' quantile regression
#'
#'@param quantile_reg
#'
plot.qreg.coeffs = function(quantile_reg, print_plot = TRUE,
                            add.significance = FALSE){

  coeff_data = suppressWarnings(lapply(names(quantile_reg),
                                       function(temp_name){

    temp_summary_list = summary(quantile_reg[[temp_name]])

    coeff_data = lapply(temp_summary_list, function(temp_summary){

      temp_df = data.frame(Tau = as.character(temp_summary$tau),
                           temp_summary$coefficients[-1,])

      temp_df$Name = rownames(temp_df)

      return(temp_df)

      }) %>%
      bind_rows()

    coeff_data$Horizon = temp_name

    return(coeff_data)

    })) %>%
    bind_rows() %>%
    setNames(str_to_title(names(.))) %>%
    mutate(Significant = (0 >= Upper.bd | 0 <= Lower.bd))

  for (temp_horizon in unique(coeff_data$Horizon)) {

    if(add.significance){

      temp_plot = ggplot(coeff_data %>%
                           filter(Horizon == temp_horizon),
                         aes(x = Tau, y = Coefficients,fill = Significant)) +
        geom_bar(stat = "identity", width = 0.25) +
        geom_hline(yintercept = 0, linetype = "dashed") +
        scale_fill_manual(values = c("TRUE" = "lightblue",
                                     "FALSE" = "lightgray")) +
        labs(title = paste(temp_horizon, "quarters ahead")) +
        theme_bw() +
        theme(legend.position = "bottom",
              plot.title = element_text(hjust = 0.5)) +
        facet_wrap(~Name)

    } else {

      temp_plot = ggplot(coeff_data %>%
                           filter(Horizon == temp_horizon),
                         aes(x = Tau, y = Coefficients)) +
        geom_bar(stat = "identity", width = 0.25) +
        geom_hline(yintercept = 0, linetype = "dashed") +
        scale_fill_manual(values = c("TRUE" = "lightblue",
                                     "FALSE" = "lightgray")) +
        labs(title = paste(temp_horizon, "quarters ahead")) +
        theme_bw() +
        theme(legend.position = "bottom",
              plot.title = element_text(hjust = 0.5)) +
        facet_wrap(~Name)


    }

    if(print_plot){

      print(temp_plot)


    } else {

      return(temp_plot)

      }

  }




}

#' Calculate rolling window predictions
#'
#' This function calculates the rolling regression predictions
#' for quantile regression
#'
#' @param reg_df dataframe
#'
#' @param win_len rolling window length
#'
#' @param quantile_vec vector of required quantiles
#'
#' @param out_of_sample_step forecasting horizon (default is next period)
#'
#' @param mod_formula model formula (default is regress the first variable
#' on everything else)
#'
rolling.qreq = function(reg_df, win_len, quantile_vec,
                            out_of_sample_step = 1,
                            mod_formula = NULL){

  if(is.null(mod_formula)){mod_formula = paste0(names(reg_df[1])," ~.")}

  n_row = nrow(reg_df)

  dates_vec = reg_df$Date

  reg_df = reg_df %>%
    select(-Date)

  out_of_sample_list = lapply(1:(n_row - win_len + 1),
         function(temp_ind){

      first_ind = temp_ind

      last_ind = win_len + temp_ind - 1

      temp_qreq = rq(formula = formula(mod_formula),
                     tau = quantile_vec,
                     data = reg_df[first_ind:last_ind,])

      temp_pred = predict(object = temp_qreq,
                          newdata = reg_df[last_ind + out_of_sample_step,])

      return(data.frame(Date = dates_vec[last_ind + out_of_sample_step],
                        temp_pred))


    })

  temp_res = out_of_sample_list %>%
    bind_rows() %>%
    filter(complete.cases(.)) %>%
    rename_at(.vars = vars(starts_with("tau")),
              .funs = list(~str_replace(.,pattern = "tau..",
                                        replacement = "")))


  return(temp_res)


}


#' @title  Calculate forecasts for GaR object
#'
#' @description This is a convenient function that calculates in sample and out of sample
#' forecast.
#' @details The in sample forecast is simply the fitted values of the regression.
#' An out of sample forecast is calculated by rolling regression (determined by \code{win_len}) that forecast
#' ahead according to \code{out_of_sample_step}
#'
get.gar.forecast = function(gar_obj, win_len, quantile_vec,
                            out_of_sample_step = 1){

  #Calculate actual values

  actual_df = gar_obj$reg_df %>%
    select(Date, starts_with("GDP_growth")) %>%
    rename_at(.vars = vars(starts_with("GDP_growth")),
              .funs = list(~str_replace(string = .,pattern = "GDP_growth_",
                                        replacement = ""))) %>%
    gather(key = Horizon,value = GaR_actual,-Date) %>%
    mutate(Quantile = "0.50")



  # Calculate in sample forecast

  forecast_in_sample = lapply(names(gar_obj$quantile_reg),
                              function(temp_name){

                                    temp_reg = gar_obj$quantile_reg[[temp_name]]

                                    col_names = gar_obj$quantile_reg[[1]] %>%
                                      coefficients() %>%
                                      colnames() %>%
                                      gsub(pattern = "tau= ",replacement = "")

                                    temp_reg$fitted.values %>%
                                      as.data.frame() %>%
                                      setNames(col_names) %>%
                                      mutate(Date = gar_obj$reg_df$Date[1:nrow(.)]) %>%
                                      gather(key = Quantile,value = GaR_forecast, -Date) %>%
                                      mutate(Horizon = temp_name)

                                  }) %>%
    bind_rows() %>%
    mutate(Date = as.yearqtr(Date)) %>%
    mutate(Forecast_Status = "In Sample") %>%
    left_join(actual_df, by = c("Date","Horizon","Quantile"))

  # Calculate out of sample forecast

  forecast_out_of_sample = lapply(parameters_list$horizon_list,
                                      function(temp_horizon){

                                        temp_var = paste("GDP_growth",temp_horizon,sep = "_")

                                        temp_roll = rolling.qreq(reg_df = gar_obj$reg_df  %>%
                                                                   select(names(.)[!grepl("GDP",names(.))],
                                                                          temp_var),
                                                                 win_len = win_len,
                                                                 quantile_vec = quantile_vec,
                                                                 mod_formula = paste0(temp_var," ~ ."),
                                                                 out_of_sample_step = out_of_sample_step)

                                        temp_roll = temp_roll %>%
                                          mutate(Horizon = as.character(temp_horizon))

                                        temp_roll = temp_roll %>%
                                          filter(complete.cases(.))

                                        return(temp_roll)

                                      }) %>%
    bind_rows() %>%
    mutate(Date = as.yearqtr(Date)) %>%
    gather(key = Quantile, value = GaR_forecast, -Date, -Horizon) %>%
    select(Date, Quantile, GaR_forecast, Horizon) %>%
    mutate(Forecast_Status = "Out of Sample") %>%
    left_join(actual_df, by = c("Date","Horizon","Quantile"))


  res_df = list(forecast_in_sample, forecast_out_of_sample) %>%
    bind_rows() %>%
    mutate(Date = as.yearqtr(Date)) %>%
    mutate(Error = GaR_actual - GaR_forecast)



  return(res_df)


}



#' @title Calculate Osnat's quantile fit score
#'
#' @description This function evaluates the goodness of fit between "realized" and to forecasted CDF
#'
quantile.fit.score = function(realized_estimate, quantile_values, quantiles){

  if(length(quantile_values) != length(quantiles)){

    stop("Quantile values and quantiles must be of same length ")

  }

  score = 0

  for (ind in seq_along(quantiles)){

    if(quantile_values[ind] < realized_estimate){

      score = score + quantiles[ind] ^ 2

    } else {

      score = score + (1 - quantiles[ind]) ^ 2

    }

  }

  return(score)

}


#' @title Make rolling window indices
#'
#' @description The function makes a grid of rolling window indices
#'
#' @param total_len = length of full grid (number of rows in data frame)
#'
#' @param win_len length of rolling window
#'
#' @param out_of_sample_step forecasting horizon
#'
#' @param win_type categorical can be either expanding or fixed. Default is fixed

make.rolling.window.grid = function(total_len ,win_len, out_of_sample_step,
                                    win_type = "fixed"){

  if(win_type == "fixed"){

    rolling_grid = lapply(1:(total_len - win_len + 1),
                              function(temp_ind){

                                first_ind = temp_ind

                                last_ind = win_len + temp_ind - 1

                                return(data.frame(First = first_ind, Last = last_ind))


                              })

  } else if (win_type == "expanding"){

    rolling_grid = lapply(1:(total_len - win_len + 1),
                          function(temp_ind){

                            first_ind = 1

                            last_ind = win_len + temp_ind - 1

                            return(data.frame(First = first_ind, Last = last_ind))


                          })

  }


  return(rolling_grid)






}
