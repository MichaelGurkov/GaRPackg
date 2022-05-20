## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(GaRPackg)
library(tidyverse)
library(zoo)

## ----import_process_data------------------------------------------------------

data("gar_data")

partitions_list = list(
  macro = c("gdp", "ind_prod_israel"),
  fin = c("credit", "house_price")
)

df = gar_data %>%
  select(date,
         starts_with("gdp"),
         unlist(partitions_list, use.names = FALSE)) %>%
  mutate(date = as.yearqtr(date)) %>%
  preprocess_df(vars_to_yoy = unlist(partitions_list, use.names = FALSE))




## ----run_forecast-------------------------------------------------------------

forecast_df = get_gar_forecast(partitions_list = partitions_list,
                               vars_df = df,
                               target_var_name = "gdp",
                               horizon_list = c(1,12),
                               quantile_vec = c(0.05,0.25,0.5,0.75,0.95))


## ----plot_fan_chart-----------------------------------------------------------

plot_fan_chart(forecast_df = forecast_df,actual_df = select(df,c("date","gdp")),
               fan_chart_date = max(df$date))


