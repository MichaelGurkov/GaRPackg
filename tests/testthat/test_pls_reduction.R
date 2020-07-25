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

temp_part = list("GDP","Ind_Prod_Israel")

result_temp_part = pls_reduction(
  df = gar_data %>%
    select(Date,unlist(temp_part)),
  target_var_name = "GDP")


expect_equal()
