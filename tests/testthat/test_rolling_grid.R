context("test rolling grid")

win_len = 3

total_len = 7

out_of_sample_step = 2

fixed_window_list = lapply(1:(total_len - win_len + 1),
                       function(temp_ind){

                         return(data.frame(First = temp_ind,
                                           Last = temp_ind + win_len - 1))

                       })

expanding_window_list = lapply(1:(total_len - win_len + 1),
                           function(temp_ind){

                             return(data.frame(First = 1,
                                               Last = temp_ind + win_len - 1))

                           })

test_that("returns proper fixed window",

  expect_equal(make.rolling.window.grid(total_len = total_len,
                                        win_len = win_len,
                                        out_of_sample_step = out_of_sample_step),
               fixed_window_list)


)


test_that("returns proper expanding window",

  expect_equal(make.rolling.window.grid(total_len = total_len,
                                        win_len = win_len,
                                        out_of_sample_step = out_of_sample_step,
                                        win_type = "expanding"),
               expanding_window_list)


)

