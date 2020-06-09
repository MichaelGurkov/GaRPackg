test_that(desc = "realized value between quantile values works",
          code = expect_equal(
            quantile.fit.score.area(
              realized_estimate = 2.5,
              quantile_values = c(1,2,3),
              quantiles = c(0.25,0.5,0.75)),
            expected = 0.8125))


test_that(desc = "realized value less than quantile works",
          code = expect_equal(
            quantile.fit.score.area(
              realized_estimate = 0.5,
              quantile_values = c(1,2,3),
              quantiles = c(0.25,0.5,0.75)),
            expected = NA))


test_that(desc = "realized value greater than quantile works",
          code = expect_equal(
            quantile.fit.score.area(
              realized_estimate = 3.5,
              quantile_values = c(1,2,3),
              quantiles = c(0.25,0.5,0.75)),
            expected = NA))
