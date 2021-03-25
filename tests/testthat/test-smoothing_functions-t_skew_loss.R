test_estimated_quantiles = c(0.05,0.5,0.95)

test_estimated_values = qt(p = test_estimated_quantiles,df = 30)

test_df = tibble(quantile = rep(test_estimated_quantiles,2),
                 values = rep(test_estimated_values,2))

test_params = c(0,1,0,30)

test_that("unequal length in quantiles and values issues error",
          expect_error(
            skew_t_loss(
              estimated_quantiles = test_estimated_quantiles[1],
              estimated_values = test_estimated_values,
              skew_t_params = test_params
            )
          ))


test_that("skew_t_loss gives zero error for t dist",
          expect_equal(
            t_skew_loss(estimated_df = test_df,
                        skew_t_params = test_params),
            0
          ))
