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

test_that("returns a tibble with coefficients",
          {
            expect_equal(
              object = test_obj %>%
                extract_coeffs_from_gar_model(),
              expected = test_obj$qreg_result %>%
                map_dfr(extract_qreq_coeff_table, .id = "horizon") %>%
                relocate(partition, horizon, quantile,
                         coeff, low, high, significant) %>%
                mutate(partition = str_remove_all(partition, "_xreg$"))
            )
          })

