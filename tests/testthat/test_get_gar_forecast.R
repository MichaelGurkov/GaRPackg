data("gar_data")

gar_data = gar_data %>%
  slice(1:35)

test_params = list()

test_params$partition = list(
  Macro = c("GDP","Ind_Prod_Israel"),
  Fincycle = c("Credit","House_Price")
  )

test_params$horizon_list = list(1,4)

test_params$quantile_vec = c(0.05,0.5)

test_params$target_var = "GDP"

test_params$win_len = 30

reg_df = make_quant_reg_df(
  partitions_list = test_params$partition,
  vars_df = gar_data,
  target_var_name = test_params$target_var,
  horizon_list = test_params$horizon_list,
  quantile_vec = test_params$quantile_vec
  )[[1]]


test_pred_df = map(
  test_params$horizon_list,
  run_cross_validation,
  reg_df = reg_df,
  target_var_name = test_params$target_var,
  quantile_vec = test_params$quantile_vec,
  win_len = test_params$win_len
  ) %>%
  bind_rows() %>%
  arrange(Date,Horizon,Quantile)

test_that("get_gar_forecast returns proper predictions",
          expect_equal(
            object = get_gar_forecast(
              partitions_list = test_params$partition,
              vars_df = gar_data,
              target_var_name = test_params$target_var,
              horizon_list = test_params$horizon_list,
              quantile_vec = test_params$quantile_vec,
              win_len = test_params$win_len) %>%
              arrange(Date,Horizon,Quantile),
            expected = test_pred_df))
