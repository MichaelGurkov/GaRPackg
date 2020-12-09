data("gar_data")



test_that("multiplication works", {
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
