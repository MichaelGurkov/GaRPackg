data("gar_data")

one_feature_part = list(
  dom_macro = list(
    "gdp"
    ),
  fin_cycle = list(
    "credit"
    )
  )

mix_feature_part = list(
  dom_macro = list(
    "gdp",
    "ind_prod_israel"
    ),
  fin_cycle = list(
    "credit"
  )
)


mult_feature_part = list(
  dom_macro = list(
    "gdp",
    "ind_prod_israel"
  ),
  fin_cycle = list(
    "credit","house_price"
  )
)


test_mix_pca = gar_data %>%
  select(date, unlist(mix_feature_part$dom_macro)) %>%
  pca_reduction()



test_mix_df = data.frame(
  date = test_mix_pca$time_index,
  dom_macro = test_mix_pca$pca_obj$x[,1]
  ) %>%
  inner_join(
    gar_data %>%
      select(date,credit) %>%
      rename(fin_cycle = credit),
    by = "date"
  ) %>%
  select(date, fin_cycle, dom_macro)



test_multi_df = map_pca_reduction(
  multi_feature_partitions = mult_feature_part,
  vars_df = gar_data, n_components = 1)


test_that("reduce_data_dimension returns one feature data",
          expect_equal(object = reduce_data_dimension(
            vars_df = gar_data,
            partition_list = one_feature_part)[[1]],
            expected = gar_data %>%
              select(date,unlist(one_feature_part))
            )
          )


test_that(paste0("reduce_data_dimension handles mix",
                 " (one and many feature) partitions"),
          expect_equal(
            object = reduce_data_dimension(
            vars_df = gar_data,
            partition_list = mix_feature_part)[[1]],
            expected = test_mix_df)
          )


test_that("reduce_data_dimension returns multi feature data",
          expect_equal(object = reduce_data_dimension(
            vars_df = gar_data,
            partition_list = mult_feature_part)[[1]],
            expected = test_multi_df$xreg_df_multi)
          )


test_that(paste0("reduce_data_dimension issues warning",
                 " when partition_list is NULL"),
          expect_warning(object = reduce_data_dimension(
            vars_df = gar_data,
            partition_list = NULL))
)


test_that(paste0("reduce_data_dimension skips reduction",
                 " object list with one feature part"),
          expect_equal(object = reduce_data_dimension(
            vars_df = gar_data,
            partition_list = one_feature_part,
            return_objects_list = TRUE)[[1]],
            expected = gar_data %>%
              select(date,unlist(one_feature_part)))
)

