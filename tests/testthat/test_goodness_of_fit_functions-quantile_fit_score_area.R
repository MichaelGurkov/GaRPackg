test_that(desc = "realized value between quantile values no outside tails",
          code = expect_equal(
            quantile_fit_score_area(
              realized_value = 2.5,
              quantile_values = c(1,2,3),
              quantiles = c(0.25,0.5,0.75),
              min_quantile_value = 1,
              max_quantile_value = 3),
            expected = 0.8125))


test_that(desc = "realized value between quantile values with outside tails",
          code = expect_equal(
            quantile_fit_score_area(
              realized_value = 2.5,
              quantile_values = c(1,2,3),
              quantiles = c(0.25,0.5,0.75),
              min_quantile_value = 0.25,
              max_quantile_value = 5),
            expected = 1.15625))


