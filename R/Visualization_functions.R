#' This function produces the fan chart plots
#'
#'
plot_fan_chart = function(forecast_df, realized_df, fan_chart_date){

  fan_chart_forecast_df = forecast_df %>%
    filter(date == fan_chart_date) %>%
    pivot_wider(values_from = "forecast_values",
                names_from = "quantile",
                names_prefix = "q_") %>%
    mutate(horizon = as.numeric(horizon))

  fan_chart_realized_df = tibble(horizon = 0:max(fan_chart_forecast_df$horizon)) %>%
    mutate(date = fan_chart_date + horizon * 0.25) %>%
    inner_join(realized_df, by = "date")

  fan_chart_forecast_df = tibble(horizon = 0,
         date = fan_chart_date,
         q_0.05 = fan_chart_realized_df$gdp[1]) %>%
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
    geom_line(data = fan_chart_realized_df,
              aes(x = horizon, y = gdp, color = "Realized GDP")) +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_x_continuous(breaks = fan_chart_realized_df$horizon) +
    xlab("Horizon (quarters)") + ylab(NULL) +
    theme(legend.title = element_blank())

}
