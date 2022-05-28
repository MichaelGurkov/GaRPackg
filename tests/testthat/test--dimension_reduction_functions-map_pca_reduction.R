
mult_part = list(part_1 = c("a","b"), part_2 = c("c","d"))

vars_df = data.frame(date = seq.Date(from = as.Date("2000-01-01"),
                                     by = "days",length.out = 10),
                     a = rnorm(10),
                     b = rnorm(10),
                     c = rnorm(10),
                     d = c(rnorm(5), rep(NA,5)))

temp = map_pca_reduction(mult_part,vars_df = vars_df,
                         n_components = 1,pca_align_list = NULL)


pca_length_vec = apply(temp$xreg_df_multi[,c("part_1","part_2")],2,
      function(temp_col){

  return(length(na.omit(temp_col)))

})

expected_length_vec = c(10,5)

names(expected_length_vec) = c("part_1","part_2")


test_that(desc = paste("map_pca_reduction applies NA filtering",
                       "to each partition of partition list"),
          code = expect_equal(object = expected_length_vec,
                              expected = expected_length_vec)
)


