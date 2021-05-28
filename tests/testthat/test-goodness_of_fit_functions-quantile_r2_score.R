test_predict_df = tribble(
  ~date, ~horizon, ~quantile, ~predicted_values,
  "2000 Q2",1,0.05,1,
  "2000 Q1",1,0.05,1,
  "2000 Q3",1,0.05,1,
  # "2000 Q4",1,0.05,1,
  "2000 Q1",8,0.05,2,
  "2000 Q2",8,0.05,2,
  "2000 Q3",8,0.05,2,
  # "2000 Q4",8,0.05,2,
  "2000 Q1",8,0.95,1,
  "2000 Q2",8,0.95,1,
  "2000 Q3",8,0.95,1,
  # "2000 Q4",8,0.95,1
)

test_benchmark_df = tribble(
  ~date, ~horizon, ~quantile, ~benchmark_values,
  "2000 Q1",1,0.05,0,
  "2000 Q2",1,0.05,0,
  "2000 Q3",1,0.05,0,
  "2000 Q4",1,0.05,0,
  "2000 Q1",8,0.05,0,
  "2000 Q2",8,0.05,0,
  "2000 Q3",8,0.05,0,
  "2000 Q4",8,0.05,0,
  "2000 Q1",8,0.95,3,
  "2000 Q2",8,0.95,3,
  "2000 Q3",8,0.95,3,
  "2000 Q4",8,0.95,3
)

test_actual_df = tribble(
  ~date,~actual_values,
  "2000 Q1",0.5,
  "2000 Q2",0.5,
  "2000 Q3",0.5,
  "2000 Q4",0.5
)


test_df = tribble(~horizon, ~ quantile,~ quantile_r2,
                  1,0.05,-18,
                  8,0.05,-56,
                  8,0.95,0.8
                  )

test_that(
  "quantile_R2_survives_numeric_example",
  code = expect_equal(
    object = quantile_r2_score(
      forecast_df = test_predict_df,
      actual_df = test_actual_df,
      benchmark_df = test_benchmark_df
    ),
    expected = test_df
  )
)

