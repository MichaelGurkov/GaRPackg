
# Complete data returns NULL

testthat::expect_equal(identify_endpoints_NA(c(3,5,8)),NULL)

# Missing values at start return list with start indices

testthat::expect_equal(identify_endpoints_NA(c(NA,NA,NA,3,5,8))
                       [["start_indices"]],c(1,2,3))

# Missing values at end return list with end indices

testthat::expect_equal(identify_endpoints_NA(c(3,5,8,NA,NA,NA))
                       [["end_indices"]],c(4,5,6))

# Missing values at endpoints return list with both indices

testthat::expect_equal(identify_endpoints_NA(c(NA,NA,NA,3,NA,NA,5,NA
                                               ,8,NA,NA,NA)),
                       list(start_indices = c(1,2,3),
                            end_indices = c(10,11,12)))


testthat::expect_equal(identify_endpoints_NA(c(NA,NA,NA,3,5,8,
                                               NA,NA,NA,NA,13,2,NA)),
                       list(start_indices = c(1,2,3),
                            end_indices = 13))

