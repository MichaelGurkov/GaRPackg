data("gar_data")

test_params = list()

test_params$horizon_list = c(4,8)

test_params$quantile_vec = c(0.05,0.25,0.5,0.75,0.95)

gar_analisys = run_GaR_analysis(
  partitions_list = list(
    dom_macro = c("gdp", "ind_prod_israel"),
    fin_cycle = c("credit", "house_price")
  ),
  vars_df = gar_data,
  target_var_name = "gdp",
  horizon_list = test_params$horizon_list,
  quantile_vec = test_params$quantile_vec
)

test_fit_skew_df = gar_analisys$fitted_df %>%
  dplyr::filter(date == as.yearqtr("2000 Q1")) %>%
  fit_t_skew_to_gar_df()


test_smoothed = extract_smoothed_quantiles(test_fit_skew_df,
                                           gar_analisys$fitted_df %>%
                                             dplyr::filter(date %in% c(as.yearqtr("2000 Q1"),
                                                                as.yearqtr("2000 Q2"))) %>%
                                             dplyr::rename(values = fitted_values))

test_that(
  "smoothing returns smoothed values where available",
  expect_equal(
    test_smoothed %>%
      dplyr::filter(date == as.yearqtr("2000 Q1")) %>%
      dplyr::pull(values_smoothed),
    test_smoothed %>%
      dplyr::filter(date == as.yearqtr("2000 Q1")) %>%
      dplyr::pull(value)
  )
)


test_that(
  "smoothing returns raw  values where smoothed unavailable",
  expect_equal(
    test_smoothed %>%
      dplyr::filter(date == as.yearqtr("2000 Q2")) %>%
      dplyr::pull(values_raw),
    test_smoothed %>%
      dplyr::filter(date == as.yearqtr("2000 Q2")) %>%
      dplyr::pull(value)
  )
)
