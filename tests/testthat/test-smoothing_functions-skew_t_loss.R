test_estimated_quantiles = c(0.05,0.05,0.95)

test_estimated_values = qt(p = test_estimated_quantiles,df = 30)

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
                   skew_t_loss(
                     estimated_quantiles = test_estimated_quantiles,
                     estimated_values = test_estimated_values,
                     skew_t_params = test_params
                   ),
                   0
                 ))
