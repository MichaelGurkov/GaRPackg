data("gar_data")

test_obj = run_GaR_analysis(
  partitions_list = list(dom_macro = c("gdp","ind_prod_israel"),
                         fin_cycle = c("credit","house_price")),
  vars_df = gar_data,
  target_var_name = "gdp",
  horizon_list = list(4),
  quantile_vec = c(0.05,0.5))


target_fitted_values = make_prediction_df(test_obj$qreg_result,
                                          test_obj$reg_df)


test_that("extract_fitted_values_from_gar_model returns proper df", {
  expect_equal(test_obj %>%
                 extract_fitted_values_from_gar_model(),
               target_fitted_values)
})
