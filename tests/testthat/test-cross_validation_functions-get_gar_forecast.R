data("gar_data")

gar_data = gar_data %>% dplyr::slice(1:31)

test_params = list()

test_params$target_var_name = "gdp"

test_params$horizon_list = list(1,4)

test_params$quantile_vec = c(0.5)

test_params$win_len = 30

test_params$win_type_expanding = TRUE

test_pred_df = suppressWarnings(run_cross_validation(
  partitions_list = test_params$partitions_list,
  vars_df = gar_data,
  target_var_name = test_params$target_var_name,
  horizon = unlist(test_params$horizon_list),
  quantile_vec = test_params$quantile_vec,
  transform_vars_df = FALSE,
  win_len = test_params$win_len,
  win_type_expanding = test_params$win_type_expanding) %>%
    arrange(date,horizon,quantile) %>%
    mutate(forecast_target_date = date + as.numeric(horizon) / 4) %>%
    relocate("forecast_target_date",.after = "forecast_values"))



test_that("get_gar_forecast returns proper predictions",
          expect_equal(
            object = suppressWarnings(get_gar_forecast(
              partitions_list = test_params$partition,
              vars_df = gar_data,
              target_var_name = test_params$target_var,
              horizon_list = test_params$horizon_list,
              quantile_vec = test_params$quantile_vec,
              transform_vars_df = FALSE,
              win_len = test_params$win_len) %>%
                arrange(date,horizon,quantile)),
            expected = test_pred_df))

