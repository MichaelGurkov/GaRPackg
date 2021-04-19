data("gar_data")

gar_data = gar_data %>%
  mutate(date = as.yearqtr(date))



test_that("all preprocess transformations work", {
  expect_equal(
    object = gar_data %>%
      preprocess_df(
        vars_to_yoy = c("gdp", "ind_prod_israel"),
        vars_to_percent_changes = c("sp500","dxy"),
        vars_to_diff = "boi_rate",
        vars_to_4_ma = "ind_prod_euro",
        convert_to_percent_units = TRUE
      ),
    expected = gar_data %>%
      mutate(across(
        c("gdp", "ind_prod_israel"), ~ (./ lag(., 4) - 1) * 100
      )) %>%
      mutate(across(c("boi_rate"), ~ c(NA, diff(.)))) %>%
      mutate(across(
        c("ind_prod_euro"),
        ~ slide_dbl(., mean, .before = 3, .complete = TRUE)
      )) %>%
      mutate(across(
        c("sp500","dxy"),
        ~ (. / lag(.) - 1) * 100
      ))
  )
})


expect_error(gar_data %>%
                 select(gdp) %>%
                 preprocess_df(vars_to_yoy = c("gdp","ind_prod_israel")),
               "date variable is missing")

expect_warning(gar_data %>%
                 select(date, gdp) %>%
                 preprocess_df(vars_to_yoy = c("gdp","ind_prod_israel")),
               "The following variables are missing : ind_prod_israel")

expect_warning(gar_data %>%
                 select(date, gdp) %>%
                 preprocess_df(vars_to_diff = c("gdp","ind_prod_israel")),
               "The following difference variables are missing : ind_prod_israel")

expect_warning(gar_data %>%
                 select(date, gdp) %>%
                 preprocess_df(vars_to_4_ma = c("gdp","ind_prod_israel")),
               "The following moving average variables are missing : ind_prod_israel")

