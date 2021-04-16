test_forecast_dist_df = tribble(~date, ~horizon,~parameter,~value,
                                1,1,"xi",0,
                                1,1,"omega",1,
                                1,1,"alpha",0,
                                1,1,"nu",Inf,
                                2,1,"xi",0,
                                2,1,"omega",1,
                                2,1,"alpha",0,
                                2,1,"nu",30,
                                2,4,"xi",0,
                                2,4,"omega",1,
                                2,4,"alpha",0,
                                2,4,"nu",Inf)

test_actual_df = tribble(~date,~target,
                         1,0,
                         2,0.2)

expected_result_df = tribble(~date,~horizon,~actual_value,~prob,
                             1,1,0,dnorm(0),
                             2,1,0.2,dt(0.2,df = 30),
                             2,4,0.2,dnorm(0.2))

test_that(
  "prediction survives basic numeric example",
  expect_equal(
    object = quantile_prediction_score(forecast_dist_df = test_forecast_dist_df,
                                       actual_df = test_actual_df),
    expected = expected_result_df
  )
)
