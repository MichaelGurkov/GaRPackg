data("gar_data")

mult_parts = list(
  Dom_Macro = list(
    "gdp",
    "Ind_Prod_Israel"
  ),
  FinCycle = list(
    "credit",
    "house_price"
  )
)

mult_parts_unnamed = list(
  list(
    "gdp",
    "Ind_Prod_Israel"
  ),
  list(
    "credit",
    "house_price"
  )
)


target_list = map(mult_parts, function(temp_part){

  return(pls_reduction(df = gar_data %>%
                         select(date,"gdp", unlist(temp_part)),
                       target_var_name = "gdp"))


})

xreg_df_multi = map2(names(results_list),results_list,
     function(temp_name,temp_obj){

  temp_df = data.frame(date = temp_obj$time_index)

  temp_df = temp_df %>%
    mutate(!!sym(temp_name) := temp_obj$pls_obj$scores[,1])

}) %>%
  reduce(inner_join, by = "date")

names(xreg_df_multi$Dom_Macro) = NULL

names(xreg_df_multi$FinCycle) = NULL


target_obj = list(xreg_df_multi = xreg_df_multi,
                  reduction_objects_list = target_list)

test_that("map_pls_reduction maps over one partition",
          expect_equal(object = map_pls_reduction(
            multi_feature_partitions = mult_parts,
            vars_df = gar_data,
            target_var_name = "gdp",
            n_components = 1),
            expected = target_obj
          ))

test_that("map_pls_reduction returns error for unnamed partition",
          expect_error(object = map_pls_reduction(
            multi_feature_partitions = mult_parts_unnamed,
            vars_df = gar_data,
            target_var_name = "gdp",
            n_components = 1)
            )
          )


