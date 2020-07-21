data("gar_data")

gar_data = gar_data %>% slice(1:40)

test_params = list()

test_params$target_var_name = "GDP"

test_params$horizon_list = list(4)

test_params$quantile_vec = c(0.5)

test_params$win_len = 30

# Make reg df

test_reg_df = make.quant.reg.df(
  partitions_list = list(Macro = c("GDP","Ind_Prod_Israel"),
                         FinCycle = c("Credit","House_Price")),
  vars_df = gar_data,
  target_var_name = "GDP",
  horizon_list = test_params$horizon_list,
  quantile_vec = test_params$quantile_vec)[["reg_df"]]



# Run quantile reg

test_quant_reg_list = run.quant.reg(
  reg_df = test_reg_df[1:test_params$win_len,],
  target_var_name = test_params$target_var_name,
  quantile_vec = test_params$quantile_vec,
  horizon_list = test_params$horizon_list)

# Predict out of sample

test_pred = predict(
  test_quant_reg_list$`4`,
  newdata = test_reg_df[test_params$win_len +
                          unlist(test_params$horizon_list),]
  )

names(test_pred) = NULL


cross_validation_pred = run.cross.validation(
  reg_df = test_reg_df,
  target_var_name = test_params$target_var_name,
  horizon = unlist(test_params$horizon_list),
  quantile_vec = test_params$quantile_vec,
  win_len = test_params$win_len,
  win_type_expanding = FALSE)


test_that(
  "run.cross.validation produces correct prediction",
  expect_equal(
    object = cross_validation_pred$GaR_forecast[1],
    expected = test_pred)
  )
