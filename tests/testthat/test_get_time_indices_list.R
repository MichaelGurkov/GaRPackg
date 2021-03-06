
context("get time index function")

dates_vec = seq.Date(from = as.Date("2000-01-01"),
                     to = as.Date("2005-01-01"),
                     by = "year")

df = data.frame(date = dates_vec,
                X = c(1,2,3,NA,5,6),
                Y = c(1,2,NA,4,5,6),
                Z = c(1,2,3,NA,5,6))

dates_list = list(dates_vec[!is.na(df$X)], dates_vec[!is.na(df$Y)])

testthat::expect_equal(get.time.indices.list(df), dates_list)
