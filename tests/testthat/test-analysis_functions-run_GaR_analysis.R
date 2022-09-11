data("gar_data")

test_params = list()

test_params$horizon_list = c(4,8)

test_params$quantile_vec = c(0.05,0.5)

test_obj = run_GaR_analysis(
  partitions_list = list(dom_macro = c("gdp","ind_prod_israel"),
                         fin_cycle = c("credit","house_price")),
  vars_df = gar_data,
  target_var_name = "gdp",
  horizon_list = test_params$horizon_list,
  quantile_vec = test_params$quantile_vec)




test_that(desc = "run_GaR_analysis returns fitted values",
          expect_equal(
            object = test_obj$fitted_df %>%
              dplyr::select(-date) %>%
              dplyr::filter(horizon == test_params$horizon_list[1]),
            expected = stats::predict(test_obj$qreg_result[[1]],
                               test_obj$reg_df) %>%
              as.data.frame() %>%
              setNames(test_params$quantile_vec) %>%
              tidyr::pivot_longer(cols = everything(),
                           names_to = "quantile",
                           values_to = "fitted_values") %>%
              dplyr::mutate(horizon = as.character(test_params$horizon_list[1])) %>%
              dplyr::mutate(quantile = as.numeric(quantile)) %>%
              dplyr::select(horizon, quantile, fitted_values) %>%
              arrange(quantile)))

