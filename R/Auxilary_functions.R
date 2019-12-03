#' This is a convinent function that encupsulates the GaR analysis
#'
#' @importFrom stats formula
#'
#' @param partititions_list list of partitons
#'
#' @param vars_df data frame with input variables
#'
#' @param method string a method that aggregates the data to partitions
#'
#'
run.GaR.analysis = function(partitions_list, vars_df,horizon_list,
                            quantile_vec,method = "inner_join_PCA"){

  # Make PCA

  pca_obj = lapply(partitions_list[sapply(partitions_list,length) > 1],
         function(temp_part){
           available_names = names(vars_df)

           part_df = vars_df %>%
             select(unlist(temp_part)[unlist(temp_part) %in% available_names],
                    Date) %>%
             filter_all(all_vars(is.finite(.)))

           ret_list = list(pca = part_df %>%
                             select(-Date) %>%
                             prcomp(.,center = TRUE,scale. = TRUE) %>%
                             align.pca(1),
                           date_index = part_df$Date)

            return(ret_list)

                           })


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
                 select(Date, GDP_F), by = "Date")

  for(temp_horizon in unlist(horizon_list)){

    temp_var = paste("GDP_growth",temp_horizon,sep = "_")

    reg_df = reg_df %>%
      mutate(!!sym(temp_var) := lead(GDP_F,temp_horizon))

  }


  # Run quantile regression

  qreg_result = lapply(horizon_list, function(temp_horizon){

    dep_var = paste0("GDP_growth_", temp_horizon)

    qreg_list = rq(formula = formula(paste0(dep_var,"~.")),
                   tau = quantile_vec,
                   data = reg_df %>%
                     select(-Date) %>%
                     select(names(.)[!grepl("GDP",names(.))], dep_var))

    return(qreg_list)


  })

  names(qreg_result) = horizon_list


  return(list(pca = pca_obj, reg_df = reg_df, quantile_reg = qreg_result))

}

#'This is a convinience functions that plots the coefficients of
#'quantile regression
#'
#'@param quantile_reg
#'
plot.qreg.coeffs = function(quantile_reg, print_plot = TRUE){

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

    if(print_plot){

      print(temp_plot)


    } else {

      return(temp_plot)

      }

  }




}


#' This function calculates the rolling regression predictions
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
