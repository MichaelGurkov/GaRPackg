set.seed(123)

test_prediction_df = expand.grid(
  date = seq.Date(from = as.Date("2000-01-01"),
                  by = "days",
                  length.out = 5),
  horizon = c(1,8),
  quantile = c(0.05,0.5)) %>%
  mutate(actual_values = ceiling(runif(20,1,10))) %>%
  mutate(predicted_values = ceiling(runif(20,1,10)))

test_that(paste0("quantile_pit_score survives",
                 " balanced numeric example"),
          expect_equal(
            object = quantile.pit.score(test_prediction_df),
            expected = test_prediction_df %>%
              mutate(pit = if_else(
                actual_values < predicted_values,
                1/(5),0)) %>%
              group_by(horizon,quantile) %>%
              summarise(pit = sum(pit), .groups = "drop")

                       )
          )

