data("gar_data")

gar_data = gar_data %>%
  slice(1:35)

test_params = list()

test_params$partition = list(
  Macro = c("gdp","Ind_Prod_Israel"),
  Fincycle = c("credit","house_price")
  )

test_params$horizon_list = list(1,4)

test_params$quantile_vec = c(0.05,0.5)

test_params$target_var = "gdp"

test_params$win_len = 30

reg_df = make.quant.reg.df(
  partitions_list = test_params$partition,
  vars_df = gar_data,
  target_var_name = test_params$target_var,
  horizon_list = test_params$horizon_list,
  quantile_vec = test_params$quantile_vec
  )[[1]]


test_pred_df = map(
  test_params$horizon_list,
  run.cross.validation,
  reg_df = reg_df,
  target_var_name = test_params$target_var,
  quantile_vec = test_params$quantile_vec,
  win_len = test_params$win_len
  ) %>%
  bind_rows() %>%
  arrange(date,horizon,quantile)

test_that("get.gar.forecast returns proper predictions",
          expect_equal(
            object = get.gar.forecast(
              partitions_list = test_params$partition,
              vars_df = gar_data,
              target_var_name = test_params$target_var,
              horizon_list = test_params$horizon_list,
              quantile_vec = test_params$quantile_vec,
              win_len = test_params$win_len) %>%
              arrange(date,horizon,quantile),
            expected = test_pred_df))
