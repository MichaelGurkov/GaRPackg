
test_that("function fills NA at previous 4 obs",
          expect_equal(c(rep(1,5), rep(2,5)),
                       fill_na_average(c(rep(1,4),NA, rep(2,4),NA))))


test_that("function doesn't fill NA outside specified window",
          expect_equal(c(rep(1,2),NA, rep(2,4),2),
                       fill_na_average(c(rep(1,2),NA, rep(2,4),NA))))


test_that("function doesn't fill NA if specified window includes NA",
          expect_equal(c(rep(1,4),1, rep(2,3),NA),
                       fill_na_average(c(rep(1,4),NA, rep(2,3),NA))))
