data("gar_data")

my_win_len = 25

start_date = as.yearqtr("2000 Q1")

end_date = start_date + (my_win_len - 1) * 0.25

my_horizon = 4

my_reg_df = gar_data %>%
  select(date, gdp) %>%
  add_leads_to_target_var("gdp",4) %>%
  rename(gdp_xvar = gdp) %>%
  filter(date >= start_date & date <= end_date + 0.25 * (my_horizon + 1))

temp = run_cross_validation(reg_df = my_reg_df,
                            target_var_name = "gdp",
                            horizon = my_horizon,
                            quantile_vec = 0.5,
                            win_len = my_win_len)

test_that(
  "run_cross_validation sets proper out of sample step",
  expect_equal(
    object = temp$date,
    expected = as.character(end_date + 0.25 * (my_horizon + 1))
  )
)
