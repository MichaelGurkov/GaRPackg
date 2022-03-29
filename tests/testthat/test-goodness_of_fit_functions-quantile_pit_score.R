set.seed(123)

test_forecast_df_quarterly = tibble::tribble(
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


test_forecast_df_monthly = tibble::tribble(
  ~date, ~horizon, ~quantile, ~predicted_values,
  "Jan 2000",1,0.05,0,
  "Jan 2000",1,0.05,0,
  "Jan 2000",8,0.05,0,
  "Jan 2000",8,0.05,0,
  "Jan 2000",1,0.95,0,
  "Jan 2000",1,0.95,0,
  "Jan 2000",8,0.95,0,
  "Jan 2000",8,0.95,0
)


test_actual_df_quarterly = tibble::tribble(
  ~date,~actual_values,
  "2000 Q2",-1,
  "2002 Q1",-1
)


test_actual_df_monthly = tibble::tribble(
  ~date,~actual_values,
  "Feb 2000",-1,
  "Sep 2000",-1
)



test_that(paste0("quantile_pit_score survives",
                 " balanced numeric example at quartely frequency"),
          expect_equal(
            object = quantile_pit_score(forecast_df = test_forecast_df_quarterly,
                                        actual_df = test_actual_df_quarterly),
            expected = test_forecast_df_quarterly %>%
              dplyr::mutate(date = as.yearqtr(date) + as.numeric(horizon) / 4) %>%
              dplyr::left_join(test_actual_df_quarterly %>%
                          dplyr::mutate(date = as.yearqtr(date)), by = "date") %>%
              dplyr::mutate(pit = if_else(
                actual_values < predicted_values,
                1/2,0)) %>%
              dplyr::group_by(horizon,quantile) %>%
              dplyr::summarise(pit = sum(pit), .groups = "drop")


          )
)


test_that(paste0("quantile_pit_score survives",
                 " balanced numeric example at monthly frequency"),
          expect_equal(
            object = quantile_pit_score(forecast_df = test_forecast_df_monthly,
                                        actual_df = test_actual_df_monthly),
            expected = test_forecast_df_monthly %>%
              dplyr::mutate(date = as.yearmon(date) + as.numeric(horizon) / 12) %>%
              dplyr::left_join(test_actual_df_monthly %>%
                          dplyr::mutate(date = as.yearmon(date)), by = "date") %>%
              dplyr::mutate(pit = if_else(
                actual_values < predicted_values,
                1/2,0)) %>%
              dplyr::group_by(horizon,quantile) %>%
              dplyr::summarise(pit = sum(pit), .groups = "drop")


          )
)


test_that(paste0("quantile_pit_score issues error",
                 " on wrong horizon class"),
          expect_error(
            object = quantile_pit_score(forecast_df = test_forecast_df_monthly %>%
                                          dplyr::mutate(horizon = factor(horizon)),
                                        actual_df = test_actual_df_monthly))
)


test_that(paste0("quantile_pit_score issues error",
                 " on wrong quantile class"),
          expect_error(
            object = quantile_pit_score(forecast_df = test_forecast_df_monthly %>%
                                          dplyr::mutate(quantile = factor(quantile)),
                                        actual_df = test_actual_df_monthly))
)

