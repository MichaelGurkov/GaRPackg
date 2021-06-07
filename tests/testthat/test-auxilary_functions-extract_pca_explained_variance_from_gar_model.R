data("gar_data")

test_params = list()

test_params$horizon_list = c(4,8)

test_params$quantile_vec = c(0.05,0.5)

test_obj = run_GaR_analysis(
  partitions_list = list(dom_macro = c("gdp","ind_prod_israel"),
                         fin_cycle = c("credit","ta125_close")),
  vars_df = gar_data,
  target_var_name = "gdp",
  horizon_list = test_params$horizon_list,
  quantile_vec = test_params$quantile_vec)

test_that(
  "extract_pca_exlained_variance_from_gar_model returns proper df length",
  code = expect_equal(
    test_obj %>%
      extract_pca_exlained_variance_from_gar_model(n_comp = 2) %>%
      nrow(),
    4
  )
)

test_that(
  "extract_pca_exlained_variance_from_gar_model returns proper names",
  code = expect_equal(
    test_obj %>%
      extract_pca_exlained_variance_from_gar_model(n_comp = 2) %>%
      names(),
    c("partition", "component", "explained_variance")
  )
)
