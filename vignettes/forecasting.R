## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------

library(GaRPackg)

library(dplyr)


## ----import_process_data------------------------------------------------------

data("gar_data")

partitions_list = list(
  macro = c("gdp_yoy", "ind_prod_israel_yoy"),
  fin = c("credit_yoy", "house_price_yoy")
)



## ----run_forecast-------------------------------------------------------------

forecast_df = get_gar_forecast(partitions_list = partitions_list,
                               vars_df = gar_data,
                               target_var_name = "gdp_yoy",
                               horizon_list = c(1,12),
                               quantile_vec = c(0.05,0.25,0.5,0.75,0.95))


## ----plot_fan_chart, fig.width=6----------------------------------------------

actual_df = gar_data %>% 
  select(date, gdp) %>% 
  mutate(actual_values = gdp/lag(gdp,4) - 1)

plot_fan_chart(forecast_df = forecast_df,
               actual_df = actual_df,
               fan_chart_date = max(actual_df$date))


## ----preprocess_data----------------------------------------------------------

preprocess_data = preprocess_df(df = gar_data,
                                partitions_list = partitions_list)

explicit_preprocessing_result = get_gar_forecast(
  partitions_list = partitions_list,
  vars_df = preprocess_data,
  target_var_name = "gdp_yoy",
  horizon_list = c(1,12),
  quantile_vec = c(0.05,0.25,0.5,0.75,0.95),
  transform_vars_df = FALSE
)

all.equal(forecast_df, explicit_preprocessing_result)


