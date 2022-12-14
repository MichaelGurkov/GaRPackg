data("gar_data")

test_params = list()

test_params$linear_part = list(
  # gdp = list(
  #   "gdp"
  # ),
  credit = list(
    "credit"
  ),
  house_price = list(
    "house_price"
))

test_params$horizon_list = list(1,4)

test_params$quantile_vec = 0.5

test_params$target_var_name = "gdp"

xreg_df = make_quant_reg_df(
  partitions_list = test_params$linear_part,
  vars_df = gar_data,
  transform_vars_df = FALSE,
  target_var_name = test_params$target_var_name,
  horizon_list = test_params$horizon_list)[[1]]

test_reg = purrr::map(test_params$horizon_list,
                 function(temp_horizon){

  x_mat = xreg_df %>%
    dplyr::select(dplyr::ends_with("_xreg"),
           paste0(test_params$target_var_name,
                  "_",
                  temp_horizon)) %>%
    dplyr::filter(complete.cases(.)) %>%
    dplyr::select(dplyr::ends_with("_xreg"))

  y_vec = xreg_df %>%
      dplyr::select(paste0(test_params$target_var_name,
                    "_",
                    temp_horizon)) %>%
      dplyr::filter(complete.cases(.))



    temp_reg = rq.fit.lasso(
    x = as.matrix(x_mat),
    y = as.matrix(y_vec),
    tau = test_params$quantile_vec,
    lambda = 2)



})

names(test_reg) = test_params$horizon_list

result_reg = run_quant_reg(
  reg_df = xreg_df,
  target_var_name = test_params$target_var_name,
  quantile_vec = test_params$quantile_vec,
  horizon_list = test_params$horizon_list,
  reg_type = "lasso",
  lambda = 2)

test_that("run_quant_reg returns quantile lasso",
          expect_equal(object = result_reg,
                       expected = test_reg))
