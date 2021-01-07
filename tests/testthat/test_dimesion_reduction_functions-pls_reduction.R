date_vec = seq.Date(from = as.Date("2000-01-01"),
                    by = "days", length.out = 12)

test_df = data.frame(date = date_vec,
                     x = c(NA,1:10,NA),
                     y = rep(22,12) + rnorm(12),
                     z = c(NA,10:1,NA))

result_df = pls_reduction(df = test_df,
                          target_var_name = "y",
                          center = FALSE, scale = FALSE)

test_that("pls reduction returns complete obs date vector",
          expect_equal(
            object = result_df$time_index,
            expected = date_vec[c(-1,-12)]
          ))


data("gar_data")

temp_part = list("gdp","ind_prod_israel")

result_temp_part = pls_reduction(
  df = gar_data %>%
    select(date,unlist(temp_part)),
  target_var_name = "gdp")

pls_form = formula("gdp ~ ind_prod_israel")

scale = TRUE

center = TRUE


test_df = gar_data %>%
  select(-date) %>%
  plsr(
    formula = pls_form,
    validation = "none",
    scale = scale,
    center = center,
    data = .
  )



expect_equal(object = result_temp_part$pls_obj,
             expected = test_df)
