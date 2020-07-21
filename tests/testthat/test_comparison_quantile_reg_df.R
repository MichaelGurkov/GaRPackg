# context("compare quantile reg df")
#
# rmarkdown::render(paste0("C:\\Users\\Misha\\Documents\\GaRPackg\\",
#             "vignettes\\Replication-Partition.Rmd"))
#
# romain_quant_df = read.csv(paste0("C:\\Users\\Misha\\Documents\\SandBox\\",
#                                             "quant_reg_df_",12,".csv"),
#                                      stringsAsFactors = FALSE) %>%
#   mutate(date = as.Date(date)) %>%
#   select(-real_gdp_nis_y,-real_gdp_yoy) %>%
#   filter(date <= as.Date("2018-09-30"))
#
#
# michael_quant_df = lapply(c(1,4,8,12), function(temp_h){
#
#   var_name = paste0("gdp_growth_fwd_", temp_h)
#
#   temp_df = make.quant.reg.df(partitions_df = partitions_df %>%
#                                 select(-real_gdp_nis_y),
#                               dep_var_df = partitions_df %>%
#                                 select(date, real_gdp_nis_y) %>%
#                                 rename(!!sym(var_name) := real_gdp_nis_y),
#                               horizon = temp_h)
#
#
# })
#
# temp = lapply(michael_quant_df[-1],
#                           function(temp_list){temp_list %>%
#                               select(date, starts_with("gdp_growth_fwd"))}) %>%
#   reduce(full_join) %>%
#   full_join(michael_quant_df[[1]])
#
#
# col_names = c("gdp_growth_fwd_1","gdp_growth_fwd_4","gdp_growth_fwd_8",
#               "gdp_growth_fwd_12")
#
# all.equal(romain_quant_df %>%
#             select(date, col_names),
#           temp %>%
#             select(date, col_names))
