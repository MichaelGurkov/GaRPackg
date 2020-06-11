test_that("basic numeric example works",
          expect_equal(round(
            quantile.r2.score(
            realized_estimate = 0,
            quantile_values = c(-0.02,0.03,0.07),
            quantile_probs = c(0.05,0.5,0.95),
            intercept_quantile_values = c(-0.01,0.02,0.03)),
            4),
            -1.1184))
