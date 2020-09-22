data("gar_data")

linear_part = list(
  gdp = list(
    "gdp"
  ),
  credit = list(
    "credit"
  ),
  house_price = list(
    "house_price"
  )
)

result_df = make.quant.reg.df(
  partitions_list = linear_part,
  vars_df = gar_data,
  target_var_name = "gdp",
  horizon_list = list(1,4),
  quantile_vec = c(0.05,0.5))

test_that(paste("make.quant.reg.df return proper names"),
          expect_equal(
            object = names(result_df$reg_df),
            expected = c("date","gdp","gdp_xreg",
                         "credit_xreg","house_price_xreg",
                         "gdp_1","gdp_4"))
            )
