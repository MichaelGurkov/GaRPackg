data("gar_data")

test_df = gar_data %>%
  select(date,gdp) %>%
  mutate(gdp_ma = c(rep(NA,3),zoo::rollmean(gdp,4)))

test_that("calculate.four.quarters.ma gets the correct lags",
          expect_equal(
            object = gar_data %>%
              select(date,gdp) %>%
              mutate(gdp_ma = calculate.four.quarters.ma(gdp)),
            expected = test_df))


