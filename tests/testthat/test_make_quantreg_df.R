data("gar_data")

linear_part = list(
  GDP = list(
    "GDP"
  ),
  Credit = list(
    "Credit"
  ),
  HousePrice = list(
    "House_Price"
  )
)

result_df = GaRPackg::make_quant_reg_df(
  partitions_list = linear_part,
  vars_df = gar_data,
  target_var_name = "GDP",
  horizon_list = list(1,4),
  quantile_vec = c(0.05,0.5))

test_that(paste("make_quant_reg_df return proper names"),
          expect_equal(
            object = names(result_df$reg_df),
            expected = c("Date","GDP","GDP_xreg",
                         "Credit_xreg","HousePrice_xreg",
                         "GDP_1","GDP_4"))
            )
