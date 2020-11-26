test_that(desc = paste0("realized value between quantile",
                        " values with outside tails"),
          code = expect_equal(
            round(quantile_crps_score(
              realized_value = 2.5,
              quantile_values = c(1,2,3),
              quantiles = c(0.25,0.5,0.75),
              min_quantile_value = 0.25,
              max_quantile_value = 5),
              4),
            expected = 0.4115))


test_that(desc = "realized value less than quantile values",
          code = expect_equal(
            quantile_crps_score(
              realized_value = 0.5,
              quantile_values = c(1,2,3),
              quantiles = c(0.25,0.5,0.75),
              min_quantile_value = 1,
              max_quantile_value = 3),
            expected = NA))

test_that(desc = "realized value greater than quantile values",
          code = expect_equal(
            quantile_crps_score(
              realized_value = 5.5,
              quantile_values = c(1,2,3),
              quantiles = c(0.25,0.5,0.75),
              min_quantile_value = 1,
              max_quantile_value = 3),
            expected = NA))


