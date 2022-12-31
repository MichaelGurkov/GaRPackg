data("gar_data")


partitions_list = list(
  macro = c("gdp_yoy", "ind_prod_israel_yoy"),
  fin = c("credit_yoy", "house_price_yoy")
)

horizon_list = c(1,4)

quantile_vec = c(0.05,0.5,0.95)

result = run_GaR_analysis(
  partitions_list = partitions_list,
  vars_df = gar_data,
  target_var_name = "gdp_yoy",
  horizon_list = horizon_list,
  quantile_vec = quantile_vec
)

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


test_that("internal and explicit_preprocessing return same results", {
  expect_equal(result, explicit_preprocessing_result)
})




forecast_df = get_gar_forecast(partitions_list = partitions_list,
                               vars_df = gar_data,
                               target_var_name = "gdp_yoy",
                               horizon_list = c(1,12),
                               quantile_vec = c(0.05,0.25,0.5,0.75,0.95),
                               win_len = 80)


explicit_preprocessing_result = get_gar_forecast(
  partitions_list = partitions_list,
  vars_df = preprocess_data,
  target_var_name = "gdp_yoy",
  horizon_list = c(1,12),
  quantile_vec = c(0.05,0.25,0.5,0.75,0.95),
  transform_vars_df = FALSE,
  win_len = 80
)

test_that(paste0("internal and explicit_preprocessing return",
                 " same out of sample results"), {
  expect_equal(forecast_df, explicit_preprocessing_result)
})
