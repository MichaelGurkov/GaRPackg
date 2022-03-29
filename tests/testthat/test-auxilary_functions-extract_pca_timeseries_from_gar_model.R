data("gar_data")

test_params = list()

test_params$horizon_list = c(4,8)

test_params$quantile_vec = c(0.05,0.5)

test_obj_with_pca = run_GaR_analysis(
  partitions_list = list(dom_macro = c("gdp","ind_prod_israel"),
                         fin_cycle = c("credit","house_price")),
  vars_df = gar_data,
  target_var_name = "gdp",
  horizon_list = test_params$horizon_list,
  quantile_vec = test_params$quantile_vec)

test_that("returns a tibble with timeseries", {
  expect_equal(object = test_obj_with_pca %>%
                 extract_pca_timeseries_from_gar_model(n_comp = 2),
               expected = test_obj_with_pca$pca_obj %>%
                 purrr::map2(., names(.), function(temp_pca, temp_name) {
                   temp_coeffs = temp_pca$pca_obj$x[, 1:2] %>%
                     as.data.frame() %>%
                     cbind(temp_pca$time_index) %>%
                     setNames(c(paste(temp_name,1:2,sep = "_"), "date")) %>%
                     dplyr::relocate("date")


                 }) %>%
                 purrr::reduce(dplyr::full_join, by = "date")
  )
})


test_obj_no_pca = run_GaR_analysis(
  partitions_list = list(dom_macro = c("gdp"),
                         fin_cycle = c("credit")),
  vars_df = gar_data,
  target_var_name = "gdp",
  horizon_list = test_params$horizon_list,
  quantile_vec = test_params$quantile_vec)

test_that(
  "gar model without pca object issues error",
  expect_error(test_obj_no_pca %>%
                 extract_pca_timeseries_from_gar_model())
)
