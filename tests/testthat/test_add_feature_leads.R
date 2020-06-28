temp = data.frame(date = seq.Date(from = as.Date("2000-01-01"),
                                  by = "day",length.out = 5), x = 1:5)

test_result = data.frame(date = seq.Date(from = as.Date("2000-01-01"),
                                         by = "day",length.out = 5),
                         x = 1:5,
                         x_lead_1 = c(2:5,NA),
                         x_lead_2 = c(3:5,rep(NA,2)),
                         x_lead_4 = c(5,rep(NA,4)))


test_that(desc = "add feature leads works",
          code = expect_equal(
            object = add.feature.leads(
              feature_name = "x",
              df = temp,
              lead_vec = c(1,2,4)
              ),
            expected = test_result)
          )
