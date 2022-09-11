data("gar_data")

gar_data = gar_data %>%
  dplyr::mutate(date = as.yearqtr(date))



test_that("all preprocess transformations work", {
  expect_equal(
    object = gar_data %>%
      preprocess_df(
        vars_to_yoy = c("gdp", "ind_prod_israel"),
        vars_to_percent_changes = c("gdp", "ind_prod_israel"),
        vars_to_diff = c("gdp", "ind_prod_israel"),
        vars_to_4_ma = c("gdp", "ind_prod_israel"),
        convert_to_percent_units = TRUE
      ),
    expected = gar_data %>%
      dplyr::mutate(across(c("gdp", "ind_prod_israel"),
                    list(yoy = ~(./ lag(., 4) - 1) * 100))) %>%
      dplyr::mutate(across(c("gdp", "ind_prod_israel"),
                    list(percent_change = ~ (. / lag(.) - 1) * 100))) %>%
      dplyr::mutate(across(c("gdp", "ind_prod_israel"),
                    list(diff = ~ c(NA, diff(.))))) %>%
      dplyr::mutate(across(c("gdp", "ind_prod_israel"),
                    list(`4_ma` = ~ slide_dbl(., mean, .before = 3,
                                            .complete = TRUE) * 100)))
  )
})


expect_error(gar_data %>%
                 dplyr::select(gdp) %>%
                 preprocess_df(vars_to_yoy = c("gdp","ind_prod_israel")),
               "date variable is missing")

expect_warning(gar_data %>%
                 dplyr::select(date, gdp) %>%
                 preprocess_df(vars_to_yoy = c("gdp","ind_prod_israel")),
               "The following variables are missing : ind_prod_israel")

expect_warning(gar_data %>%
                 dplyr::select(date, gdp) %>%
                 preprocess_df(vars_to_diff = c("gdp","ind_prod_israel")),
               "The following difference variables are missing : ind_prod_israel")

expect_warning(gar_data %>%
                 dplyr::select(date, gdp) %>%
                 preprocess_df(vars_to_4_ma = c("gdp","ind_prod_israel")),
               "The following moving average variables are missing : ind_prod_israel")

