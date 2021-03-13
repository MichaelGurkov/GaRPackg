test_estimated_quantiles = c(0.05,0.5,0.95)

test_estimated_values = qt(p = test_estimated_quantiles,df = 30)

test_df = tibble(quantile = test_estimated_quantiles,
                 values = test_estimated_values)

expected_params = c(-0.002873464,1.002900948,0.003527175,32.966185382)

names(expected_params) = c("xi","omega","alpha","nu")

test_that("t_skew_fitting_works", {
  expect_equal(fit_skew_t_distribution(estimated_df = test_df),
               expected_params)
})

test_that("t_skew_fitting_returns NA and issued warning on time out", {
  fit_skew_t_distribution(estimated_df = test_df,time_limit = 1) %>%
    expect_warning() %>%
    expect_equal(NA)
})


