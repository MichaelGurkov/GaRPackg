## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  echo = TRUE,
  comment = "#>", warning = FALSE, message = FALSE
)

## ----setup--------------------------------------------------------------------

library(GaRPackg)


## ----import_process_data------------------------------------------------------

data("gar_data")



## ---- echo=TRUE---------------------------------------------------------------

partitions_list = list(
  macro = c("gdp_yoy", "ind_prod_israel_yoy"),
  fin = c("credit_yoy", "house_price_yoy")
)

horizon_list = c(1,4)

quantile_vec = c(0.05,0.5,0.95)


## ----run_analysis-------------------------------------------------------------

result = run_GaR_analysis(
  partitions_list = partitions_list,
  vars_df = gar_data,
  target_var_name = "gdp_yoy",
  horizon_list = horizon_list,
  quantile_vec = quantile_vec
)


## ----print_results------------------------------------------------------------
names(result)

## ----preprocess_data----------------------------------------------------------

preprocess_data = preprocess_df(df = gar_data,
                                partitions_list = partitions_list)

explicit_preprocessing_result = run_GaR_analysis(
  partitions_list = partitions_list,
  vars_df = preprocess_data,
  target_var_name = "gdp_yoy",
  horizon_list = horizon_list,
  quantile_vec = quantile_vec,
  transform_vars_df = FALSE
)

all.equal(result, explicit_preprocessing_result)


