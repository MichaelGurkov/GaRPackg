data("gar_data")

test_df = gar_data %>%
  select(Date,GDP) %>%
  mutate(GDP_ma = c(rep(NA,3),zoo::rollmean(GDP,4)))

test_that("calculate_four_quarters_ma gets the correct lags",
          expect_equal(
            object = gar_data %>%
              select(Date,GDP) %>%
              mutate(GDP_ma = calculate_four_quarters_ma(GDP)),
            expected = test_df))


