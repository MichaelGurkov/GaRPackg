data("gar_data")

test_df = gar_data %>%
  select(date,gdp) %>%
  mutate(gdp_yoy = gdp / lag(gdp,4)-1)

test_that("calculate.YoY.returns gets the correct lags",
          expect_equal(
            object = gar_data %>%
              select(date,gdp) %>%
              mutate(gdp_yoy = calculate.YoY.returns(gdp)),
            expected = test_df))
