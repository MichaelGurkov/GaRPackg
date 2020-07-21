context("Quantile_fit_score")

test_that("Different length of quantile values and quantiles throws an error",
          {
            expect_error(quantile.fit.score(
              realized_estimate = 3,
              quantile_values = c(1, 2),
              quantiles = c(0.25, 0.5, 0.75)
            ))
          })

test_that("Realized estimate of 0 returns maximum value",
          {
            expect_equal(
              quantile.fit.score(
                realized_estimate = 0,
                quantile_values = c(1, 2, 3),
                quantiles = c(0.25, 0.5, 0.75)),
              sum(c(0.25, 0.5, 0.75) ^ 2))
          })


test_that("Numeric example",
          {
            expect_equal(
              quantile.fit.score(
                realized_estimate = 1.5,
                quantile_values = c(1, 2),
                quantiles = c(0.25, 0.75)
              ),
              sum(c(0.25, (1 - 0.75)) ^ 2)
            )
          })
