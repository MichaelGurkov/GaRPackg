data("gar_data")


test_that(paste0(
  "returns an error on empty data",
  "(issues a warning on missing vars)"
),
{
  get_gar_forecast(
    partitions_list = list(
      dom_macro = c("private_consumption", "public_consumption"),
      dom_fin = c("boi_rate", "ta_35_impl_vol")
    ),
    vars_df = gar_data,
    target_var_name = "gdp",
    horizon_list = c(1, 2),
    quantile_vec = c(0.05, 0.5)
  ) %>%
    expect_error() %>%
    expect_warning()
})

