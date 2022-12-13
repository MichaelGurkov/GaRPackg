temp_part = list(dom_macro = c("gdp_yoy","cpi_israel_yoy"),
                 dom_fin = c("credit_yoy"),
                 ext_fin = c("rate_euro_diff","rate_us_diff"))


expected_arguments_list = list(vars_to_yoy = c("gdp","cpi_israel",
                                               "credit"),
                               vars_to_diff = c("rate_euro","rate_us"),
                               vars_to_ma = character(0))

test_that("extract_preprocess_arguments works", {
  expect_equal(expected_arguments_list,
               extract_preprocess_arguments(temp_part))
})
