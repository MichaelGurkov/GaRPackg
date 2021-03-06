data("gar_data")

one_feature_part = list(
  Dom_Macro = list(
    "gdp"
    ),
  FinCycle = list(
    "credit"
    )
  )

mix_feature_part = list(
  Dom_Macro = list(
    "gdp",
    "Ind_Prod_Israel"
    ),
  FinCycle = list(
    "credit"
  )
)


mult_feature_part = list(
  Dom_Macro = list(
    "gdp",
    "Ind_Prod_Israel"
  ),
  FinCycle = list(
    "credit","house_price"
  )
)


test_mix_pca = gar_data %>%
  select(date, unlist(mix_feature_part$Dom_Macro)) %>%
  pca_reduction()



test_mix_df = data.frame(
  date = test_mix_pca$time_index,
  Dom_Macro = test_mix_pca$pca_obj$x[,1]
  ) %>%
  inner_join(
    gar_data %>%
      select(date,credit) %>%
      mutate(FinCycle = scale(credit)),
    by = "date"
  ) %>%
  select(date, FinCycle, Dom_Macro)



test_multi_df = map_pca_reduction(
  multi_feature_partitions = mult_feature_part,
  vars_df = gar_data, n_components = 1)


test_that("reduce_data_dimension returns one feature data",
          expect_equal(object = reduce_data_dimension(
            vars_df = gar_data,
            partition = one_feature_part)[[1]],
            expected = gar_data %>%
              select(date,unlist(one_feature_part)) %>%
              mutate(across(-date,scale))
            )
          )


test_that(paste0("reduce_data_dimension handles mix",
                 " (one and many feature) partitions"),
          expect_equal(
            object = reduce_data_dimension(
            vars_df = gar_data,
            partition = mix_feature_part)[[1]],
            expected = test_mix_df)
          )


test_that("reduce_data_dimension returns multi feature data",
          expect_equal(object = reduce_data_dimension(
            vars_df = gar_data,
            partition = mult_feature_part)[[1]],
            expected = test_multi_df$xreg_df_multi)
          )


test_that(paste0("reduce_data_dimension issues warning",
                 " when partition is NULL"),
          expect_warning(object = reduce_data_dimension(
            vars_df = gar_data,
            partition = NULL))
)


test_that(paste0("reduce_data_dimension skips reduction",
                 " object list with one feature part"),
          expect_equal(object = reduce_data_dimension(
            vars_df = gar_data,
            partition = one_feature_part,
            return_objects_list = TRUE)[[1]],
            expected = gar_data %>%
              select(date,unlist(one_feature_part)) %>%
              mutate(across(-date,scale)))
)

