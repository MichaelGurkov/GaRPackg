test_that("basic numeric example works",
          expect_equal(
            quantile.r2.score(
              realized_estimates = c(0,0,0),
              forecast_values = c(-0.02,0.03,0.07),
              quantile = 0.05,
              benchmark_values = c(-0.01,0.02,0.03)),-1))
