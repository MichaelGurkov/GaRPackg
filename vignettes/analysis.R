## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  echo = TRUE,
  comment = "#>", warning = FALSE, message = FALSE
)

## ----setup--------------------------------------------------------------------

library(GaRPackg)

library(dplyr)

library(zoo)


## ----import_process_data------------------------------------------------------

data("gar_data")


## ---- echo=TRUE---------------------------------------------------------------

partitions_list = list(
  macro = c("gdp", "ind_prod_israel"),
  fin = c("credit", "house_price")
)

horizon_list = c(1,4)

quantile_vec = c(0.05,0.5,0.95)


## ----preprocess_data----------------------------------------------------------

df = gar_data %>%
  select(date,
         unlist(partitions_list, use.names = FALSE)) %>%
  mutate(date = as.yearqtr(date)) %>%
  preprocess_df(vars_to_yoy = unlist(partitions_list,
                                     use.names = FALSE))


## ----run_analysis-------------------------------------------------------------

result = run_GaR_analysis(partitions_list = partitions_list,
                          vars_df = df,
                          target_var_name = "gdp",
                          horizon_list = horizon_list,
                          quantile_vec = quantile_vec)


## ----print_results------------------------------------------------------------
names(result)

