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
  dplyr::select(date, unlist(mix_feature_part$dom_macro)) %>%
  pca_reduction()



test_mix_df = data.frame(
  date = test_mix_pca$time_index,
  dom_macro = test_mix_pca$pca_obj$x[,1]
  ) %>%
  dplyr::inner_join(
    gar_data %>%
      dplyr::select(date,fin_cycle = credit),
    by = "date"
  ) %>%
  dplyr::select(date, fin_cycle, dom_macro)



test_multi_df = map_pca_reduction(
  multi_feature_partitions = mult_feature_part,
  vars_df = gar_data, n_components = 1)


test_that("reduce_data_dimension returns one feature data",
          expect_equal(object = reduce_data_dimension(
            vars_df = gar_data,
            partitions_list = one_feature_part)[[1]],
            expected = gar_data %>%
              dplyr::select(date,unlist(one_feature_part))
            )
          )


test_that(paste0("reduce_data_dimension handles mix",
                 " (one and many feature) partitions"),
          expect_equal(
            object = reduce_data_dimension(
            vars_df = gar_data,
            partitions_list = mix_feature_part)[[1]],
            expected = test_mix_df)
          )


test_that("reduce_data_dimension returns multi feature data",
          expect_equal(object = reduce_data_dimension(
            vars_df = gar_data,
            partitions_list = mult_feature_part)[[1]],
            expected = test_multi_df$xreg_df_multi)
          )


test_that(paste0("reduce_data_dimension returns error",
                 " when partitions_list is NULL"),
          expect_error(object = reduce_data_dimension(
            vars_df = gar_data,
            partitions_list = NULL))
)


test_that(paste0("reduce_data_dimension returns error",
                 " when preprocess_method is incorrect"),
          expect_error(object = reduce_data_dimension(
            vars_df = gar_data,
            partitions_list = mult_feature_part,
            preprocess_method = "inner_join_pca"))
)


test_that(paste0("reduce_data_dimension skips reduction",
                 " object list with one feature part"),
          expect_equal(object = reduce_data_dimension(
            vars_df = gar_data,
            partitions_list = one_feature_part,
            return_objects_list = TRUE)[[1]],
            expected = gar_data %>%
              dplyr::select(date,unlist(one_feature_part)))
)


test_that(paste0("reduce_data_dimension returns NA tibble",
                 " when partition has only missing values"),
          expect_equal(object = reduce_data_dimension(
            vars_df = gar_data %>%
              dplyr::mutate(across(unlist(mult_feature_part,
                                   use.names = FALSE),~NA)),
            partitions_list = mult_feature_part)[[1]],
            gar_data %>%
              select(date) %>%
              mutate(dom_macro = NA) %>%
              mutate(fin_cycle = NA))
)


