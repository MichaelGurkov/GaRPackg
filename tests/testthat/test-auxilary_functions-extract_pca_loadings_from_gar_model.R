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

test_that("returns a tibble with loadings", {
  expect_equal(object = test_obj_with_pca %>%
                 extract_pca_loadings_from_gar_model(),
               expected = test_obj_with_pca$pca_obj %>%
                 purrr::map_dfr(., function(temp_pca) {
                   temp_coeffs = temp_pca$pca_obj$rotation[, 1] %>%
                     as.data.frame() %>%
                     setNames("coeff") %>%
                     tibble::rownames_to_column()


                 },
                 .id = "partition")
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
                 extract_pca_loadings_from_gar_model())
)
