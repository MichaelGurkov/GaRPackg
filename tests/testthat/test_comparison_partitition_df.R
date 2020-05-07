# context("compare domestic FCI df")
#
#
# rmarkdown::render(paste0("C:\\Users\\Misha\\Documents\\GaRPackg\\",
#                          "vignettes\\Replication-Partition.Rmd"))
#
# partitions_df = partitions_df %>%
#   select(-real_gdp_nis_y) %>%
#   slice(-nrow(.))
#
# romain_partition = read.csv(paste0("C:\\Users\\Misha\\Documents\\GaR Python",
#                                     "\\Israel GaR\\Output\\Partitions\\",
#                                     "partitions_data.csv"),
#                              stringsAsFactors = FALSE) %>%
#    select(-real_gdp_nis_y,-real_gdp_yoy) %>%
#   slice(-nrow(.))
#
#
# sapply(names(partitions_df)[!names(partitions_df) == "date"],
#        function(temp_name){cor(partitions_df[[temp_name]],
#                                romain_partition[[temp_name]])})
