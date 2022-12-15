data("gar_data")

gar_data = gar_data %>%
  dplyr::mutate(date = as.yearqtr(date))



expected_transformed_df = list(
  gar_data %>%
    dplyr::mutate(across(
      c("gdp", "ind_prod_israel"),
      list(yoy = ~ (. / lag(., 4) - 1)))) %>%
    select(date, gdp_yoy, ind_prod_israel_yoy),
  gar_data %>%
    dplyr::mutate(across(
      c("gdp", "ind_prod_israel"),
      list(percent_change = ~ (. / lag(.) - 1)))) %>%
    select(date, gdp_percent_change, ind_prod_israel_percent_change),
  gar_data %>%
    dplyr::mutate(across(
      c("gdp", "ind_prod_israel"),
      list(diff = ~ c(NA, diff(.))))) %>%
    select(date, gdp_diff, ind_prod_israel_diff),
  gar_data %>%
    dplyr::mutate(across(
      c("gdp", "ind_prod_israel"),
      list(`4_ma` = ~ slide_dbl(
        ., mean, .before = 3,
        .complete = TRUE
      )))) %>%
    select(date, gdp_4_ma, ind_prod_israel_4_ma)) %>%
  reduce(full_join, by = "date") %>%
  filter(complete.cases(.))



test_that("all preprocess transformations work", {
  expect_equal(
    object = gar_data %>%
      preprocess_df(
        vars_to_yoy = c("gdp", "ind_prod_israel"),
        vars_to_percent_change = c("gdp", "ind_prod_israel"),
        vars_to_diff = c("gdp", "ind_prod_israel"),
        vars_to_4_ma = c("gdp", "ind_prod_israel")),
    expected = expected_transformed_df)})



partitions_list = list(first = c("gdp_yoy", "ind_prod_israel_yoy"),
                      second = c("gdp_percent_change",
                                 "ind_prod_israel_percent_change"),
                      third = c("gdp_diff", "ind_prod_israel_diff"),
                      fourth = c("gdp_4_ma", "ind_prod_israel_4_ma"))


test_that("all preprocess transformations work with partition list",{
  expect_equal(
    object = gar_data %>%
      preprocess_df(partitions_list = partitions_list),
    expected = expected_transformed_df)})


test_that("preprocess returns raw df in case of no transformations",{
  expect_equal(
    object = gar_data %>%
      preprocess_df(partitions_list =
                      list(dom_macro = c("gdp","ind_prod_israel"))),
    expected = gar_data)})


expect_error(gar_data %>%
                 dplyr::select(gdp) %>%
                 preprocess_df(vars_to_yoy = c("gdp","ind_prod_israel")),
               "date variable is missing")

expect_warning(gar_data %>%
                 dplyr::select(date, gdp) %>%
                 preprocess_df(vars_to_yoy = c("gdp","ind_prod_israel")),
               "The following variables are missing : ind_prod_israel")

