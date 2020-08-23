#' This is a convinience functions that plots the coefficients of
#' quantile regression
#'
#'
#'@param quantile_reg
#'
#'@import ggplot2
#'
plot_qreg_coeffs = function(quantile_reg,
                            print_plot = TRUE,
                            add.significance = FALSE) {
  coeff_data = suppressWarnings(lapply(names(quantile_reg),
                                       function(temp_name) {
                                         temp_summary_list = summary(quantile_reg[[temp_name]])

                                         coeff_data = lapply(temp_summary_list,
                                                             function(temp_summary) {
                                                               temp_df = data.frame(Tau = as.character(temp_summary$tau),
                                                                                    temp_summary$coefficients[-1, ])

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
    if (add.significance) {
      temp_plot = ggplot(
        coeff_data %>%
          filter(Horizon == temp_horizon),
        aes(x = Tau, y = Coefficients,
            fill = Significant)
      ) +
        geom_bar(stat = "identity", width = 0.25) +
        geom_hline(yintercept = 0, linetype = "dashed") +
        scale_fill_manual(values = c("TRUE" = "lightblue",
                                     "FALSE" = "lightgray")) +
        labs(title = paste(temp_horizon, "quarters ahead")) +
        theme_bw() +
        theme(legend.position = "bottom",
              plot.title = element_text(hjust = 0.5)) +
        facet_wrap( ~ Name)

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
        facet_wrap( ~ Name)


    }

    if (print_plot) {
      print(temp_plot)


    } else {
      return(temp_plot)

    }

  }


}
