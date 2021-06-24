data("gar_data")

test_params = list()

test_params$horizon_list = c(4,8)

test_params$quantile_vec = c(0.05,0.5)

test_obj = run_GaR_analysis(
  partitions_list = list(dom_macro = c("gdp","ind_prod_israel"),
                         fin_cycle = c("credit","house_price")),
  vars_df = gar_data,
  target_var_name = "gdp",
  horizon_list = test_params$horizon_list,
  quantile_vec = test_params$quantile_vec)


test_data_mat = test_obj$reg_df %>%
  select(ends_with("_xreg")) %>%
  as.matrix()

test_data_mat = cbind(rep(1, nrow(test_data_mat)), test_data_mat)


coeffs_df = test_obj %>%
  extract_coeffs_from_gar_model() %>%
  filter(.data$quantile == "0.5") %>%
  select(.data$coeff,.data$horizon, .data$partition)


test_factors_df = map_dfr(
  unique(coeffs_df$horizon),function(temp_horizon){

    coef_vec = coeffs_df %>%
      filter(.data$horizon == temp_horizon) %>%
      select(.data$coeff) %>%
      unlist(use.names = FALSE)

    test_factors_df =  t(t(test_data_mat) * coef_vec)

    test_factors_df = test_factors_df %>%
      as.data.frame() %>%
      cbind(date = test_obj$reg_df$date) %>%
      mutate(horizon = temp_horizon)

    return(test_factors_df)

  })%>%
  rename_all(~str_remove_all(.,"_xreg")) %>%
  rename(intercept = V1)

test_that("returns a tibble with factors for median quantile",
          {
            expect_equal(
              object = test_obj %>%
                extract_factor_contribution_from_gar_model(target_quantile = "0.5"),
              expected = test_factors_df
            )
          })


test_that("produces error if missing quantile is asked",
          expect_error(extract_factor_contribution_from_gar_model(test_obj,
                                                                  target_quantile = "0.08")))
