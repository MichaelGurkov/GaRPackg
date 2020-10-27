data("gar_data")

gar_data = gar_data %>% slice(1:40)

test_params = list()

test_params$target_var_name = "gdp"

test_params$horizon_list = list(4)

test_params$quantile_vec = c(0.5)

test_params$win_len = 30

# Make reg df

test_reg_df = make.quant.reg.df(
  partitions_list = list(Macro = c("gdp","Ind_Prod_Israel"),
                         FinCycle = c("credit","house_price")),
  vars_df = gar_data,
  target_var_name = "gdp",
  horizon_list = test_params$horizon_list,
  quantile_vec = test_params$quantile_vec)[["reg_df"]]



# Run quantile reg

test_quant_reg_list_fixed = run.quant.reg(
  reg_df = test_reg_df[1:test_params$win_len,],
  target_var_name = test_params$target_var_name,
  quantile_vec = test_params$quantile_vec,
  horizon_list = test_params$horizon_list)

test_quant_reg_list_expanding = run.quant.reg(
  reg_df = test_reg_df[1:(nrow(test_reg_df)-unlist(test_params$horizon_list)),],
  target_var_name = test_params$target_var_name,
  quantile_vec = test_params$quantile_vec,
  horizon_list = test_params$horizon_list)

# Predict out of sample

test_pred_fixed = predict(
  test_quant_reg_list_fixed$`4`,
  newdata = test_reg_df[test_params$win_len +
                          unlist(test_params$horizon_list),]
  )


test_pred_expanding = predict(
  test_quant_reg_list_expanding$`4`,
  newdata = test_reg_df[nrow(test_reg_df),]
)

names(test_pred_fixed) = NULL

names(test_pred_expanding) = NULL


cross_validation_pred_fixed = run.cross.validation(
  reg_df = test_reg_df,
  target_var_name = test_params$target_var_name,
  horizon = unlist(test_params$horizon_list),
  quantile_vec = test_params$quantile_vec,
  win_len = test_params$win_len,
  win_type_expanding = FALSE)

cross_validation_pred_expanding = run.cross.validation(
  reg_df = test_reg_df,
  target_var_name = test_params$target_var_name,
  horizon = unlist(test_params$horizon_list),
  quantile_vec = test_params$quantile_vec,
  win_len = test_params$win_len,
  win_type_expanding = TRUE)


test_that(
  "run.cross.validation produces correct prediction in fixed window",
  expect_equal(
    object = cross_validation_pred_fixed$gar_forecast[1],
    expected = test_pred_fixed)
  )


test_that(
  desc = "run.cross.validation produces correct prediction in expanding window",
  code =  expect_equal(
    object = cross_validation_pred_expanding$gar_forecast[
      nrow(cross_validation_pred_expanding)],
    expected = test_pred_expanding)
)

test_that(
  "run.cross.validation produces returns correct date",
  expect_equal(
    object = cross_validation_pred_fixed$date[1],
    expected = test_reg_df$date[test_params$win_len +
                             unlist(test_params$horizon_list)]))

test_that(
  "run.cross.validation returns scalar quantile vec",
  expect_equal(
    object = unique(cross_validation_pred_fixed$quantile),
    expected = test_params$quantile_vec))

