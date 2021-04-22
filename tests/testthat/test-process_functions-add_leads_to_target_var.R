test_df = data.frame(date = seq.Date(from = as.Date("2000-01-01"),
                                     by = "days",
                                     length.out = 10),
                     x = 1:10)


result_df = data.frame(date = seq.Date(from = as.Date("2000-01-01"),
                                     by = "days",
                                     length.out = 10),
                     x = 1:10,
                     x_1 = c(2:10,NA),
                     x_4 = c(5:10,rep(NA,4)))

test_that("add.leads.to.target.var adds lead values correctly",
          expect_equal(
            add_leads_to_target_var(test_df,"x",c(1,4)),
            result_df))
