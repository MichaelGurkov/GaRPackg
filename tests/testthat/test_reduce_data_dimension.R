data("gar_data")

one_feature_part = list(
  Dom_Macro = list(
    "GDP"
    ),
  FinCycle = list(
    "Credit"
    )
  )

mix_feature_part = list(
  Dom_Macro = list(
    "GDP",
    "Ind_Prod_Israel"
    ),
  FinCycle = list(
    "Credit"
  )
)

test_that("reduce_data_dimension returns one feature data",
          expect_equal(object = reduce_data_dimension(
            vars_df = gar_data,
            partition = one_feature_part)[[1]],
            expected = gar_data %>%
              select(Date,unlist(one_feature_part))
            )
          )


test_that("reduce_data_dimension hadles mix (one and many feature) partitions",
          expect_equal(object = reduce_data_dimension(
            vars_df = gar_data,
            partition = mix_feature_part)[[1]],
            expected = gar_data %>%
              select(Date,unlist(one_feature_part))
          )
)
