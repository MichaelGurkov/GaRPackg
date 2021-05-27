test_estimated_quantiles = c(0.05,0.5,0.95)

test_estimated_values = qt(p = test_estimated_quantiles,df = 30)

test_df = tibble(quantile = test_estimated_quantiles,
                 values = test_estimated_values)


expected_params = c(0,1,0,30)

names(expected_params) = c("xi","omega","alpha","nu")

test_that("t_skew_fitting_works_in_approximation", {
  expect_equal(round(fit_t_skew(estimated_df = test_df),1),
               expected_params)
})

expected_time_out_res = rep(0,4)

names(expected_time_out_res) = c("xi","omega","alpha","nu")

test_that("t_skew_fitting_returns 0 vector and issued warning on time out", {
  fit_t_skew(estimated_df = test_df %>%
               mutate(values = 1),time_limit = 10 ^ (-20)) %>%
    expect_warning() %>%
    expect_equal(expected_time_out_res)
})


test_that("t_skew_fitting converts to numeric and issues warning", {
  fit_t_skew(estimated_df = test_df %>%
                            mutate(across(everything(), as.character))) %>%
    round(1) %>%
    expect_warning() %>%
    expect_equal(expected_params)
})


