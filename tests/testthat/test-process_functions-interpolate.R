
raw_data = c(NA,NA,NA,3,5,8,NA,NA,NA,NA,13,2,NA)

clean_data = c(NA,NA,NA,3,5,8,9,10,11,12,13, 2, 2)

expect_equal(interpolate(raw_data), clean_data)
