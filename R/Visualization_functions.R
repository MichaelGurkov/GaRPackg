#' This function produces the fan chart plots
#'
#'
plot_fan_chart = function(forecast_df, realized_df, fan_chart_date){

  fan_chart_forecast_df = forecast_df %>%
    filter(date == fan_chart_date) %>%
    pivot_wider(values_from = "forecast_values",
                names_from = "quantile",
                names_prefix = "q_") %>%
    mutate(horizon = as.numeric(horizon)) %>%
    mutate(date = date + 0.25 * horizon)

  fan_chart_realized_df = realized_df %>%
    inner_join(select(fan_chart_forecast_df, c("horizon", "date")),
               by = "date") %>%
    mutate(horizon = as.numeric(horizon))


  ggplot() +
    geom_ribbon(
      data = fan_chart_forecast_df,
      mapping = aes(x = horizon, ymin = q_0.05, ymax = q_0.95),
      fill = "skyblue"
    ) +
    geom_ribbon(
      data = fan_chart_forecast_df,
      mapping = aes(x = horizon, ymin = q_0.25, ymax = q_0.75),
      fill = "skyblue"
    ) +
    geom_line(data=fan_chart_forecast_df,aes(x=horizon,y=q_0.50), color="skyblue4") +
    geom_line(data = fan_chart_realized_df, aes(x = horizon, y = gdp), color = "red")


}
