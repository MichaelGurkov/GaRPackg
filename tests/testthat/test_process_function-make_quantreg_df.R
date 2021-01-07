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

result_df = make_quant_reg_df(
  partitions_list = linear_part,
  vars_df = gar_data,
  target_var_name = "gdp",
  horizon_list = list(1,4))

plain_df = gar_data %>%
  select(gdp, ind_prod_israel, boi_rate) %>%
  add_leads_to_target_var(target_var_name = "gdp",
                          leads_vector = c(1,2))


test_that(paste("make_quant_reg_df return proper names"),
          expect_equal(
            object = names(result_df$reg_df),
            expected = c("date","gdp","gdp_xreg",
                         "credit_xreg","house_price_xreg",
                         "gdp_1","gdp_4"))
            )

test_that(
  'make_quant_reg_df returns data "as is"',
  expect_equal(
    object = gar_data %>%
      select(date,gdp, ind_prod_israel, boi_rate) %>%
      make_quant_reg_df(target_var_name = "gdp",
                        horizon_list = c(1, 2),
                        preprocess_method = "asis"),
    expected = gar_data %>%
      select(date,gdp, ind_prod_israel, boi_rate) %>%
      add_leads_to_target_var(target_var_name = "gdp",
                              leads_vector = c(1, 2)) %>%
      rename_at(vars(-c("date","gdp")), ~paste0(.,"_xreg"))
  )
)


test_that(
  'make_quant_reg_df issues a warning on missing vars',
  expect_warning(
    object = gar_data %>%
      select(date,gdp, ind_prod_israel, boi_rate) %>%
      make_quant_reg_df(target_var_name = "gdp",
                        horizon_list = c(1, 2),
                        partitions_list = linear_part))
  )



