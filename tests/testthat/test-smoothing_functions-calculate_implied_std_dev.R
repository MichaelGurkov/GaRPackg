

data_points_vec = c(1.3,3.25)

percentiles_vec = c(0.75, 0.95)


test_that("the function returns correct answer for specified params",
          {
  expect_equal(round(calculate_implied_std_dev(data_points_vec,
                                               percentiles_vec),3),
               1.976)
})
