data("gar_data")

test_df = gar_data %>%
  select(Date,GDP) %>%
  mutate(GDP_yoy = GDP / lag(GDP,4)-1)

test_that("calculate_YoY_returns gets the correct lags",
          expect_equal(
            object = gar_data %>%
              select(Date,GDP) %>%
              mutate(GDP_yoy = calculate_YoY_returns(GDP)),
            expected = test_df))
