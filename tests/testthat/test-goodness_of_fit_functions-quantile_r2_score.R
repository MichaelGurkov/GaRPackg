test_predict_df_quarterly = tribble(
  ~date, ~horizon, ~quantile, ~predicted_values,
  "1999 Q4",1,0.05,1,
  "2000 Q1",1,0.05,1,
  "2000 Q2",1,0.05,1,
  "1998 Q1",8,0.05,2,
  "1998 Q2",8,0.05,2,
  "1998 Q3",8,0.05,2,
  "1998 Q1",8,0.95,1,
  "1998 Q2",8,0.95,1,
  "1998 Q3",8,0.95,1,
)


test_predict_df_monthly = tribble(
  ~date, ~horizon, ~quantile, ~predicted_values,
  "Jan 1999",1,0.05,1,
  "Feb 1999",1,0.05,1,
  "Mar 1999",1,0.05,1,
  "Jun 1999",8,0.05,2,
  "Jul 1999",8,0.05,2,
  "Aug 1999",8,0.05,2,
  "Jun 1999",8,0.95,1,
  "Jul 1999",8,0.95,1,
  "Aug 1999",8,0.95,1,
)

test_benchmark_df_quarterly = tribble(
  ~date, ~horizon, ~quantile, ~benchmark_values,
  "1999 Q4",1,0.05,0,
  "2000 Q2",1,0.05,0,
  "2000 Q2",1,0.05,0,
  "2000 Q3",1,0.05,0,
  "1998 Q1",8,0.05,0,
  "1998 Q2",8,0.05,0,
  "1998 Q3",8,0.05,0,
  "1998 Q4",8,0.05,0,
  "1998 Q1",8,0.95,3,
  "1998 Q2",8,0.95,3,
  "1998 Q3",8,0.95,3,
  "1998 Q4",8,0.95,3
)

test_benchmark_df_monthly = tribble(
  ~date, ~horizon, ~quantile, ~benchmark_values,
  "Jan 1999",1,0.05,0,
  "Feb 2000",1,0.05,0,
  "Feb 2000",1,0.05,0,
  "Mar 2000",1,0.05,0,
  "Jan 1998",8,0.05,0,
  "Feb 1998",8,0.05,0,
  "Mar 1998",8,0.05,0,
  "Apr 1998",8,0.05,0,
  "Jan 1998",8,0.95,3,
  "Feb 1998",8,0.95,3,
  "Mar 1998",8,0.95,3,
  "Apr 1998",8,0.95,3
)

test_actual_df_quarterly = tribble(
  ~date,~actual_values,
  "2000 Q1",0.5,
  "2000 Q2",0.5,
  "2000 Q3",0.5,
  "2000 Q4",0.5
)


test_actual_df_monthly = tribble(
  ~date,~actual_values,
  "Feb 1999",0.5,
  "Mar 1999",0.5,
  "Apr 1999",0.5,
  "May 1999",0.5
)


test_df_quarterly = tribble(~horizon, ~ quantile,~ quantile_r2,
                  1,0.05,-18,
                  8,0.05,-56,
                  8,0.95,0.8
                  )

test_that(
  "quantile_R2_survives_numeric_example at quarter frequency",
  code = expect_equal(
    object = quantile_r2_score(
      forecast_df = test_predict_df_quarterly,
      actual_df = test_actual_df_quarterly,
      benchmark_df = test_benchmark_df_quarterly,
      frequency = "quarterly"
    ),
    expected = test_df_quarterly
  )
)

test_that(
  "quantile_R2_survives_numeric_example at monthly frequency",
  code = expect_equal(
    object = quantile_r2_score(
      forecast_df = test_predict_df_quarterly,
      actual_df = test_actual_df_quarterly,
      benchmark_df = test_benchmark_df_quarterly,
      frequency = "monthly"
    ),
    expected = test_df_quarterly
  )
)

