

test_that("zoo object identification works",
          code = {
            expect_equal(object = identify_frequency(as.yearqtr("2000 Q1")),
                         expected =  "quarterly")
            expect_equal(object = identify_frequency(as.yearmon("Jan 2000")),
                         expected =  "monthly")
          })


test_that("character identification works",
          code = {
            expect_equal(object = identify_frequency("2000 Q1"),
                         expected =  "quarterly")
            expect_equal(object = identify_frequency("Jan 2000"),
                         expected =  "monthly")
          })
