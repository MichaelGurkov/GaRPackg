data("gar_data")

test_params = list()

test_params$horizon_list = c(4,8)

test_params$quantile_vec = c(0.05,0.25,0.5,0.75,0.95)

gar_analisys = run_GaR_analysis(
  partitions_list = list(
    dom_macro = c("gdp", "ind_prod_israel"),
    fin_cycle = c("credit", "house_price")
  ),
  vars_df = gar_data %>%
    mutate(date = as.yearqtr(date)) %>%
    preprocess_df(
      vars_to_yoy = c("gdp", "ind_prod_israel", "credit", "house_price")
    ),
  target_var_name = "gdp",
  horizon_list = test_params$horizon_list,
  quantile_vec = test_params$quantile_vec
)

test_fit_skew_df = gar_analisys$gar_fitted_df %>%
  filter(date == as.yearqtr("2000 Q1")) %>%
  fit_t_skew_to_gar_df()

test_smoothed_df = gar_analisys$gar_fitted_df %>%
  filter(date == as.yearqtr("2000 Q1")) %>%
  fit_t_skew_to_gar_df(return_smoothed_quantiles = TRUE)

test_that("fit_t_skew_to_gar_df returns proper structure for dist params", {
  expect_equal(unique(test_fit_skew_df$t_skew_parameter),
               c("xi", "omega", "alpha", "nu"))
  expect_equal(names(test_fit_skew_df),
               c("date", "horizon", "t_skew_parameter", "values"))
})


test_that("fit_t_skew_to_gar_df returns proper structure for smoothed quantiles", {
  expect_equal(names(test_smoothed_df),
               c("date","quantile", "horizon", "values_smoothed","values_raw", "value"))
})
