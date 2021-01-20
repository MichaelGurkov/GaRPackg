data("gar_data")

gar_data = gar_data %>%
  mutate(date = as.yearqtr(date))


test_that("all preprocess transformations work", {
  expect_equal(
    object = gar_data %>%
      select(date, gdp, ind_prod_israel,ind_prod_euro, boi_rate, vix) %>%
      preprocess_df(
        vars_to_yoy = c("gdp", "ind_prod_israel"),
        vars_to_diff = "boi_rate",
        vars_to_4_ma = "ind_prod_euro"
      ),
    expected = gar_data %>%
      select(date, gdp, ind_prod_israel,ind_prod_euro, boi_rate, vix) %>%
      mutate(across(
        c("gdp", "ind_prod_israel"), ~ ./ lag(., 4) - 1
      )) %>%
      mutate(across(c("boi_rate"), ~ c(NA, diff(.)))) %>%
      mutate(across(
        c("ind_prod_euro"),
        ~ slide_dbl(., mean, .before = 3, .complete = TRUE)
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

