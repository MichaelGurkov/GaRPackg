date_vec = seq.Date(from = as.Date("2000-01-01"),
                    by = "days", length.out = 12)

test_df = data.frame(date = date_vec,
                     x = c(NA,1:10,NA), y = rep(0,12))

result_df = pca_reduction(df = test_df,center = FALSE, scale = FALSE)

test_that("pca reduction returns complete obs date vector",
          expect_equal(
            object = result_df$time_index,
            expected = date_vec[c(-1,-12)]
          ))

test_that("pca reduction returns complete obs pca",
          expect_equal(
            object = abs(result_df$pca_obj$x[,1]),
            expected = test_df$x[c(-1,-12)]
          ))
