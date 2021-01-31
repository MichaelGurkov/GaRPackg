data("gar_data")

test_params = list()

test_params$horizon_list = c(4,8)

test_params$quantile_vec = c(0.05,0.5)

test_obj = run_GaR_analysis(
  partitions_list = list(Dom_Macro = c("gdp"),
                         FinCycle = c("credit")),
  vars_df = gar_data,
  target_var_name = "gdp",
  horizon_list = test_params$horizon_list,
  quantile_vec = test_params$quantile_vec)

test_that(desc = "run_GaR_analysis returns fitted values",
          expect_equal(
            object = test_obj$gar_fitted_df %>%
              select(-date) %>%
              filter(Horizon == test_params$horizon_list[1]),
            expected = predict(test_obj$qreg_result[[1]],
                               test_obj$reg_df[1:89,]) %>%
              as.data.frame() %>%
              setNames(test_params$quantile_vec) %>%
              pivot_longer(cols = everything(),
                           names_to = "Quantile",
                           values_to = "GaR_fitted") %>%
              mutate(Horizon = as.character(test_params$horizon_list[1])) %>%
              arrange(Quantile)))

