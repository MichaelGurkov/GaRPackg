data("gar_data")

gar_data = gar_data %>% dplyr::slice(1:40)

test_params = list()

test_params$target_var_name = "gdp"

test_params$horizon_list = list(4)

test_params$quantile_vec = c(0.5)

test_params$win_len = 30

# Make reg df

test_reg_df = make_quant_reg_df(
  partitions_list = list(macro = c("gdp","ind_prod_israel"),
                         fin_cycle = c("credit","house_price")),
  vars_df = gar_data,
  target_var_name = "gdp",
  horizon_list = test_params$horizon_list)[["reg_df"]]


# Run quantile reg

test_quant_reg_list_fixed = run_quant_reg(
  reg_df = test_reg_df[1:test_params$win_len,],
  target_var_name = test_params$target_var_name,
  quantile_vec = test_params$quantile_vec,
  horizon_list = test_params$horizon_list)

test_quant_reg_list_expanding = run_quant_reg(
  reg_df = test_reg_df[1:(test_params$win_len + 1),],
  target_var_name = test_params$target_var_name,
  quantile_vec = test_params$quantile_vec,
  horizon_list = test_params$horizon_list
)


# Predict out of sample

test_pred_fixed = stats::predict(
  test_quant_reg_list_fixed$`4`,
  newdata = test_reg_df[test_params$win_len,]
  )


test_pred_expanding = stats::predict(
  test_quant_reg_list_expanding$`4`,
  newdata = test_reg_df[test_params$win_len + 1,]
)

names(test_pred_fixed) = NULL

names(test_pred_expanding) = NULL


cross_validation_pred_fixed = run_cross_validation(
  reg_df = test_reg_df,
  target_var_name = test_params$target_var_name,
  horizon = unlist(test_params$horizon_list),
  quantile_vec = test_params$quantile_vec,
  win_len = test_params$win_len,
  win_type_expanding = FALSE)

cross_validation_pred_expanding = run_cross_validation(
  reg_df = test_reg_df,
  target_var_name = test_params$target_var_name,
  horizon = unlist(test_params$horizon_list),
  quantile_vec = test_params$quantile_vec,
  win_len = test_params$win_len,
  win_type_expanding = TRUE)


test_that(
  "run_cross_validation produces correct prediction in fixed window",
  expect_equal(
    object = cross_validation_pred_fixed$forecast_values[1],
    expected = test_pred_fixed)
  )


test_that(
  desc = "run_cross_validation produces correct prediction in expanding window",
  code =  expect_equal(
    object = cross_validation_pred_expanding$forecast_values[2],
    expected = test_pred_expanding)
)

test_that(
  "run_cross_validation produces returns correct date",
  expect_equal(
    object = cross_validation_pred_fixed$date[1],
    expected = test_reg_df$date[test_params$win_len]))

test_that(
  "run_cross_validation returns scalar quantile vec",
  expect_equal(
    object = unique(cross_validation_pred_fixed$quantile),
    expected = test_params$quantile_vec))

