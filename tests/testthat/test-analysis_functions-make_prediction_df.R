
data("gar_data")

test_multiple_quantile = run_GaR_analysis(
  partitions_list = list(dom_macro = c("gdp","ind_prod_israel"),
                         fin_cycle = c("credit","house_price")),
  vars_df = gar_data,
  target_var_name = "gdp",
  horizon_list = list(4),
  quantile_vec = c(0.05,0.5))


target_multiple_quantile = predict.rq(test_multiple_quantile$qreg_result[[1]],
                            newdata = test_multiple_quantile$reg_df) %>%
  as_tibble() %>%
  mutate(horizon = "4") %>%
  bind_cols(select(test_multiple_quantile$reg_df,"date")) %>%
  pivot_longer(-c("date","horizon"), names_to = "quantile",
               values_to = "fitted_values") %>%
  mutate(quantile = str_remove(quantile, "tau= ")) %>%
  mutate(quantile = as.numeric(quantile)) %>%
  select(date, horizon, quantile, fitted_values) %>%
  arrange(quantile, date)



test_that("make_prediction_df returns correct predictions", {
  expect_equal(test_multiple_quantile$fitted_df, target_multiple_quantile)
})


test_single_quantile = run_GaR_analysis(
  partitions_list = list(dom_macro = c("gdp","ind_prod_israel"),
                         fin_cycle = c("credit","house_price")),
  vars_df = gar_data,
  target_var_name = "gdp",
  horizon_list = list(4),
  quantile_vec = 0.05)



target_single_quantile = predict.rq(test_single_quantile$qreg_result[[1]],
                                      newdata = test_single_quantile$reg_df) %>%
  as_tibble() %>%
  mutate(horizon = "4") %>%
  bind_cols(select(test_multiple_quantile$reg_df,"date")) %>%
  mutate(quantile = 0.05) %>%
  select(date, horizon, quantile, fitted_values = value)


test_that("make_prediction_df returns correct predictions for single quantile", {
  expect_equal(test_single_quantile$fitted_df, target_single_quantile)
})
