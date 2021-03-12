set.seed(123)


test_forecast_df = tribble(
  ~date, ~horizon, ~quantile, ~predicted_values,
  "2000 Q1",1,0.05,0,
  "2000 Q1",1,0.05,0,
  "2000 Q1",8,0.05,0,
  "2000 Q1",8,0.05,0,
  "2000 Q1",1,0.95,0,
  "2000 Q1",1,0.95,0,
  "2000 Q1",8,0.95,0,
  "2000 Q1",8,0.95,0
)

test_actual_df = tribble(
  ~date,~actual_values,
  "2000 Q2",-1,
  "2002 Q1",-1
)







test_that(paste0("quantile_pit_score survives",
                 " balanced numeric example"),
          expect_equal(
            object = quantile_pit_score(forecast_df = test_forecast_df,
                                        actual_df = test_actual_df),
            expected = test_forecast_df %>%
              mutate(date = as.yearqtr(date) + as.numeric(horizon) * 0.25) %>%
              left_join(test_actual_df %>%
                          mutate(date = as.yearqtr(date)), by = "date") %>%
              mutate(pit = if_else(
                actual_values < predicted_values,
                1/2,0)) %>%
              group_by(horizon,quantile) %>%
              summarise(pit = sum(pit), .groups = "drop")


          )
)
