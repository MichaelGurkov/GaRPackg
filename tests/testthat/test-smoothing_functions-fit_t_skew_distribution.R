test_estimated_quantiles = c(0.05,0.5,0.95)

test_estimated_values = qt(p = test_estimated_quantiles,df = 30)

test_df = tibble(quantile = test_estimated_quantiles,
                 values = test_estimated_values)

timed_out_vector = c(0,0,0,0)

names(timed_out_vector) =  c("xi","omega","alpha","nu")

expected_params = c(0,1,0,30)

names(expected_params) = c("xi","omega","alpha","nu")


test_that("t_skew_fitting_returns vector and issues a warning on time out",
          {
            fit_skew_t_distribution(estimated_df = test_df,
                                    time_limit = (1 * 10 ^ (-2))) %>%
              expect_warning() %>%
              expect_equal(timed_out_vector)
          })


test_that("t_skew_fitting_works", {
  expect_equal(fit_skew_t_distribution(estimated_df = test_df) %>%
                 round(),
               expected_params)
})
