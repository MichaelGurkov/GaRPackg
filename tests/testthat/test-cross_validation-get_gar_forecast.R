test_that("get gar forecast returns an error on empty data", {
  expect_error(
    get_gar_forecast(
      partitions_list = list(
        dom_macro = c("private_consumption", "public_consumption"),
        dom_fin = c("boi_rate", "ta_35_impl_vol")
      ),
      vars_df = gar_data,
      target_var_name = "gdp",
      horizon_list = c(1, 2),
      quantile_vec = c(0.05, 0.5)
    )
  )
})
