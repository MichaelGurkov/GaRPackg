data("gar_data")

test_params = list()

test_params$horizon_list = c(4,8)

test_params$quantile_vec = c(0.05,0.5,0.9)

test_obj = run_GaR_analysis(
  partitions_list = list(dom_macro = c("gdp"),
                         fin_cycle = c("credit")),
  vars_df = gar_data,
  target_var_name = "gdp",
  horizon_list = test_params$horizon_list,
  quantile_vec = test_params$quantile_vec)




test_that(
  desc = "calculate_skew_and_iqr returns proper object",
  expect_equal(
    object = test_obj %>%
      calculate_skew_and_iqr(quantile_values = c(0.05, 0.5, 0.9)) %>%
      colnames(),
    expected = c("date", "horizon", "skew", "iqr")
  )
)


test_that(
  desc = "calculate_skew_and_iqr stops on error if quantiles are missing",
  expect_error(
    object = test_obj %>%
      calculate_skew_and_iqr(),
    regexp = "the following quantile(s) are missing in the model object: 0.25,0.75",
    fixed = TRUE
  )
)

