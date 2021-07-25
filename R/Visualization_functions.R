#' This function produces the fan chart plots
#'
#' @inheritParams quantile_r2_score
#'
#' @param fan_chart_date a date of the initial time point in the fan chart.
#'  The default is the last date of forecast df
#'
#' @export
plot_fan_chart = function(forecast_df, actual_df, fan_chart_date = NULL){

  horizon = q_0.05 = q_0.25 =  q_0.5 = q_0.75 = q_0.95 = gdp = NULL

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package \"ggplot2\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if("fitted_values" %in% names(forecast_df)){

    forecast_df = forecast_df %>%
      rename(forecast_values = fitted_values)
  }

  if(is.null(fan_chart_date)){fan_chart_date = max(forecast_df$date)}

  if(!fan_chart_date %in% forecast_df$date){

    stop("fan_chart_date is not found in forecast_df")
  }



  fan_chart_forecast_df = forecast_df %>%
    filter(date == fan_chart_date) %>%
    mutate(quantile = as.numeric(quantile)) %>%
    pivot_wider(values_from = "forecast_values",
                names_from = "quantile",
                names_prefix = "q_") %>%
    mutate(horizon = as.numeric(horizon))

  time_frequency = identify_frequency(forecast_df$date)

  if(time_frequency == "quarterly"){

    fan_chart_actual_df = tibble(horizon = 0:max(fan_chart_forecast_df$horizon)) %>%
      mutate(date = fan_chart_date + horizon / 4) %>%
      left_join(actual_df, by = "date")

  } else if(time_frequency == "monthly"){

    fan_chart_actual_df = tibble(horizon = 0:max(fan_chart_forecast_df$horizon)) %>%
      mutate(date = fan_chart_date + horizon / 12) %>%
      left_join(actual_df, by = "date")


  }

  fan_chart_forecast_df = tibble(horizon = 0,
                                 date = fan_chart_date,
                                 q_0.05 = fan_chart_actual_df$gdp[1]) %>%
    mutate(q_0.25 = q_0.05) %>%
    mutate(q_0.5 = q_0.05) %>%
    mutate(q_0.75 = q_0.05) %>%
    mutate(q_0.95 = q_0.05) %>%
    bind_rows(fan_chart_forecast_df)


  fan_plot = ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data = fan_chart_forecast_df,
      mapping = ggplot2::aes(x = horizon, ymin = q_0.05, ymax = q_0.95),
      fill = "skyblue",
      alpha = 0.5
    ) +
    ggplot2::geom_ribbon(
      data = fan_chart_forecast_df,
      mapping = ggplot2::aes(x = horizon, ymin = q_0.25, ymax = q_0.75),
      fill = "skyblue",
      alpha = 0.75
    ) +
    ggplot2::geom_line(data = fan_chart_forecast_df,
                       ggplot2::aes(x = horizon, y = q_0.5), color =
                "skyblue4") +
    ggplot2::geom_line(data = fan_chart_actual_df,
                       ggplot2::aes(x = horizon, y = gdp, color = "Realized GDP")) +
    ggplot2::geom_point(data = fan_chart_actual_df,
                        ggplot2::aes(x = horizon, y = gdp, color = "Realized GDP")) +
    ggplot2::scale_y_continuous(labels = scales::percent_format()) +
    ggplot2::scale_x_continuous(breaks = fan_chart_actual_df$horizon) +
    ggplot2::xlab("Horizon (quarters)") + ggplot2::ylab(NULL) +
    ggplot2::theme(legend.title = ggplot2::element_blank())


  suppressWarnings(suppressMessages(print(fan_plot)))

}
