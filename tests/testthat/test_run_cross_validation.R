set.seed(123)

test_df = data.frame(Date = 1:10,
                     x = rnorm(10),
                     y = rnorm(10)) %>%
  mutate(y_2 = lead(y,2))

test_reg = rq(
  formula = "y_2~x",
  tau = 0.5,
  data = test_df[1:5,])

test_pred = predict(test_reg, newdata = test_df[7,])

names(test_pred) = NULL

cross_validation_pred = run.cross.validation(
  reg_df = test_df,
  target_var_name = "y",
  horizon = 2,
  quantile_vec = 0.5,
  win_len = 5,
  win_type_expanding = FALSE)


test_that(
  "run.cross.validation produces correct prediction",
  expect_equal(
    object = cross_validation_pred$GaR_forecast[1],
    expected = test_pred)
  )
