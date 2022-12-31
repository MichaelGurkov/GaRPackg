data("gar_data")

gar_data = gar_data %>% dplyr::slice(1:40)

test_params = list()

test_params$target_var_name = "gdp"

test_params$horizon_list = list(1,4)

test_params$quantile_vec = c(0.5)

test_params$win_len = 30

test_params$partitions_list = list(
  macro = c("gdp","ind_prod_israel"),
  fin_cycle = c("credit","house_price"))

# Make reg df

test_reg_df_30 = make_quant_reg_df(
  partitions_list = test_params$partitions_list,
  vars_df =  slice(gar_data,1:30),
  target_var_name = "gdp",
  transform_vars_df = FALSE,
  horizon_list = test_params$horizon_list)[["reg_df"]]


test_reg_df_31 = make_quant_reg_df(
  partitions_list = test_params$partitions_list,
  vars_df =  slice(gar_data,1:31),
  target_var_name = "gdp",
  transform_vars_df = FALSE,
  horizon_list = test_params$horizon_list)[["reg_df"]]


# Run quantile reg

test_quant_reg_30 = run_quant_reg(
  reg_df = test_reg_df_30,
  target_var_name = test_params$target_var_name,
  quantile_vec = test_params$quantile_vec,
  horizon_list = test_params$horizon_list)

test_quant_reg_31 = run_quant_reg(
  reg_df = test_reg_df_31,
  target_var_name = test_params$target_var_name,
  quantile_vec = test_params$quantile_vec,
  horizon_list = test_params$horizon_list)


# Predict out of sample

test_pred_fixed = stats::predict(test_quant_reg_30$`4`,
                                 newdata = test_reg_df_30[30,])


test_pred_expanding = c(stats::predict(test_quant_reg_30$`4`,
                                       newdata = test_reg_df_30[30, ]),
                        stats::predict(test_quant_reg_31$`4`,
                                       newdata = test_reg_df_31[31, ]))

names(test_pred_fixed) = NULL

names(test_pred_expanding) = NULL


cross_validation_pred_fixed_30 = run_cross_validation(
  partitions_list = test_params$partitions_list,
  vars_df = slice(gar_data,1:30),
  target_var_name = test_params$target_var_name,
  horizon = unlist(test_params$horizon_list),
  quantile_vec = test_params$quantile_vec,
  transform_vars_df = FALSE,
  win_len = test_params$win_len,
  win_type_expanding = FALSE)

cross_validation_pred_expanding_30_31 = run_cross_validation(
  partitions_list = test_params$partitions_list,
  vars_df = slice(gar_data,1:31),
  target_var_name = test_params$target_var_name,
  horizon = unlist(test_params$horizon_list),
  quantile_vec = test_params$quantile_vec,
  transform_vars_df = FALSE,
  win_len = test_params$win_len,
  win_type_expanding = TRUE)


error_df = gar_data[1:31,]

error_df[31,!names(error_df) == "date"] = NA

cross_validation_pred_na = run_cross_validation(
  partitions_list = test_params$partitions_list,
  vars_df = error_df,
  target_var_name = test_params$target_var_name,
  horizon = unlist(test_params$horizon_list),
  quantile_vec = test_params$quantile_vec,
  transform_vars_df = FALSE,
  win_len = test_params$win_len,
  win_type_expanding = TRUE)



test_that(
  "run_cross_validation produces correct prediction in fixed window",
  expect_equal(
    object = cross_validation_pred_fixed_30$forecast_values[
      cross_validation_pred_fixed_30$horizon == 4],
    expected = test_pred_fixed)
  )


test_that(
  desc = "run_cross_validation produces correct prediction in expanding window",
  code =  expect_equal(
    object = cross_validation_pred_expanding_30_31$forecast_values[
      cross_validation_pred_expanding_30_31$horizon == 4],
    expected = test_pred_expanding)
)

test_that(
  "run_cross_validation produces returns correct date",
  expect_equal(
    object = cross_validation_pred_fixed_30$date[
      cross_validation_pred_fixed_30$horizon == 4],
    expected = test_reg_df_30$date[test_params$win_len]))

test_that(
  "run_cross_validation returns scalar quantile vec",
  expect_equal(
    object = unique(cross_validation_pred_fixed_30$quantile[
      cross_validation_pred_fixed_30$horizon == 4]),
    expected = test_params$quantile_vec))


test_that(
  "run_cross_validation accepts date in win_len",
  expect_equal(
    object = run_cross_validation(
      partitions_list = test_params$partitions_list,
      vars_df = slice(gar_data,1:30),
      target_var_name = test_params$target_var_name,
      horizon = unlist(test_params$horizon_list),
      quantile_vec = test_params$quantile_vec,
      transform_vars_df = FALSE,
      win_len = gar_data$date[30],
      win_type_expanding = FALSE),
    expected = cross_validation_pred_fixed_30))


test_that(
  "run_cross_validation returns NA with missing predictors data",
  expect_true(
    object = all(is.na(cross_validation_pred_na$forecast_values[3:4]))))

test_that(
  "run_cross_validation returns error with too large win_len",
  expect_error(run_cross_validation(
    partitions_list = test_params$partitions_list,
    vars_df = error_df,
    target_var_name = test_params$target_var_name,
    horizon = unlist(test_params$horizon_list),
    quantile_vec = test_params$quantile_vec,
    transform_vars_df = FALSE,
    win_len = 150,
    win_type_expanding = TRUE)))
