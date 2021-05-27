test_df = data.frame(date = seq.Date(from = as.Date("2000-01-01"),
                                     to = as.Date("2008-01-01"),
                                     by = "year"), X = 1:9)

out_vec =  c(((test_df$X[5:9]/ test_df$X[1:5]) - 1), rep(NA,4))

testthat::expect_equal(calculate_CAGR(test_df,4),
                       data.frame(date = test_df$date, X = out_vec))
