data("gar_data")

one_feature_part = list(
  Dom_Macro = list("GDP"),
  FinCycle = list(
    "Credit"
    )
  )

test_that("reduce_data_dimension returns one feature data",
          expect_equal(object = reduce_data_dimension(
            vars_df = gar_data,
            partition = one_feature_part)[[1]],
            expected = gar_data %>%
              select(unlist(one_feature_part))
            )
          )
