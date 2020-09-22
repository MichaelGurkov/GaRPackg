test_df = data.frame(date = c(rep(1:6,each = 3)),
                     Quantile = as.character(rep(c(0.05,0.5,0.95),2)),
                     Horizon = rep(c(1,4,8),each = 6),
                     GaR_forecast = c(1,2,3,9,5,8,10,7,6,1,2,3,9,5,8,10,7,6))

result_df = data.frame(date = c(rep(1:6,each = 3)),
                       Quantile = as.character(rep(c(0.05,0.5,0.95),2)),
                       Horizon = rep(c(1,4,8),each = 6),
                       GaR_forecast = c(1,2,3,5,8,9,6,7,10,1,2,3,5,8,9,6,7,10)) %>%
  as_tibble()


test_that(
  desc = "quantile crossing is fixed",
  code = expect_equal(
    object = test_df %>%
      fix.quantile.crossing() %>%
      arrange(date, Horizon,Quantile),
    expected = result_df
    )
  )


