data("gar_data")

test_df = gar_data %>%
  dplyr::select(date,gdp) %>%
  dplyr::mutate(gdp_ma = c(rep(NA,3),zoo::rollmean(gdp,4)))

test_that("calculate_four_quarters_ma gets the correct lags",
          expect_equal(
            object = gar_data %>%
              dplyr::select(date,gdp) %>%
              dplyr::mutate(gdp_ma = calculate_four_quarters_ma(gdp)),
            expected = test_df))


