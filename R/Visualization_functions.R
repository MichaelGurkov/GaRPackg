

#' This function produces the fan chart plots
#'
#' @importFrom scales percent_format
#'
#' @importFrom ggplot2 ggplot aes geom_ribbon element_blank geom_line
#' geom_point scale_x_continuous scale_y_continuous theme xlab ylab
#'
#' @param forecast_df data frame with predicted values
#' by horizon, quantile, date, forecast_values. The date is the forecast date,
#' on which the forecast was made, the target date is calculated by taking the
#' forecast date \code{horizon} steps ahead. Currently quarterly and monthly
#' frequency is supported.
#'
#' @param actual_df data frame with actual values
#' by date and actual_values
#'
#' @param fan_chart_date a date of the initial time point in the fan chart
#'
#' @export
plot_fan_chart = function(forecast_df, actual_df, fan_chart_date){

  horizon = q_0.05 = q_0.25 =  q_0.50 = q_0.75 = q_0.95 = gdp = NULL

  if(!fan_chart_date %in% forecast_df$date){

    stop("fan_chart_date is not found in forecast_df")
  }

  fan_chart_forecast_df = forecast_df %>%
    filter(date == fan_chart_date) %>%
    pivot_wider(values_from = "forecast_values",
                names_from = "quantile",
                names_prefix = "q_") %>%
    mutate(horizon = as.numeric(horizon))

  fan_chart_actual_df = tibble(horizon = 0:max(fan_chart_forecast_df$horizon)) %>%
    mutate(date = fan_chart_date + horizon * 0.25) %>%
    left_join(actual_df, by = "date")

  fan_chart_forecast_df = tibble(horizon = 0,
                                 date = fan_chart_date,
                                 q_0.05 = fan_chart_actual_df$gdp[1]) %>%
    mutate(q_0.25 = q_0.05) %>%
    mutate(q_0.50 = q_0.05) %>%
    mutate(q_0.75 = q_0.05) %>%
    mutate(q_0.95 = q_0.05) %>%
    bind_rows(fan_chart_forecast_df)


  ggplot() +
    geom_ribbon(
      data = fan_chart_forecast_df,
      mapping = aes(x = horizon, ymin = q_0.05, ymax = q_0.95),
      fill = "skyblue",
      alpha = 0.5
    ) +
    geom_ribbon(
      data = fan_chart_forecast_df,
      mapping = aes(x = horizon, ymin = q_0.25, ymax = q_0.75),
      fill = "skyblue",
      alpha = 0.75
    ) +
    geom_line(data = fan_chart_forecast_df, aes(x = horizon, y = q_0.50), color =
                "skyblue4") +
    geom_line(data = fan_chart_actual_df,
              aes(x = horizon, y = gdp, color = "Realized GDP")) +
    geom_point(data = fan_chart_actual_df,
              aes(x = horizon, y = gdp, color = "Realized GDP")) +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_x_continuous(breaks = fan_chart_actual_df$horizon) +
    xlab("Horizon (quarters)") + ylab(NULL) +
    theme(legend.title = element_blank())

}
