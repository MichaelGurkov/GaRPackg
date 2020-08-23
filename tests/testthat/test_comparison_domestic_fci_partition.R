# context("compare domestic FCI partitions")
#
#
# #Import my df
#
# df = read.csv(paste0("C:\\Users\\Misha\\Documents\\GaR Python\\Israel",
#                      " GaR\\Data\\Final\\final_data_israel.csv"),
#               stringsAsFactors = FALSE) %>%
#   mutate(date = as.Date(date)) %>%
#   filter(date >= as.Date("1996-01-01") & date <= as.Date("2019-06-01")) %>%
#   select(c("date",'term_spread', 'corporate_spread',
#           'interbank_spread','sovereign_spread',
#           'securities_ba_spread',
#           'equity_returns_qoq', 'policy_rate',
#           'cpi_inflation',
#           'implied_yield_cpi_bond_yoy',
#           'inflation_expectations',
#           'fx_returns_qoq', 'fx_ba_spread',
#           'equity_implied_vol'))
#
#
# df_interpolated = df %>%
#   mutate_at(vars(-date), interpolate)
#
# time_indices_list = get_time_indices_list(df_interpolated)
#
# time_indices_list = time_indices_list[order(sapply(time_indices_list,
#                                                    length))]
#
# dates_components = lapply(time_indices_list,function(temp_ind){
#
#   temp_df = df_interpolated %>%
#     filter(date %in% temp_ind) %>%
#     select_if(~sum(is.na(.)) == 0)
#
# })
#
# dates_components = dates_components[order(sapply(dates_components, nrow),
#                                           decreasing = TRUE)]
#
# agg_series = lapply(dates_components, function(temp_comp){
#
#   temp_comp %>%
#     pca_reduction() %>%
#     mutate(PCA = scale(PCA))
#
#
# })
#
# agg_series = agg_series[order(sapply(agg_series, nrow))]
#
#
# diff_series = lapply(agg_series, function(temp_agg_series){
#
#   temp_agg_series %>%
#     mutate(PCA = PCA - lead(PCA)) %>%
#     slice(-nrow(.))
#
#
# })
#
# diff_series = diff_series[order(sapply(diff_series, nrow))]
#
#
# # Import romain df
#
# romain_dates_components = lapply(1:6, function(temp_ind){
#
#   temp_df = read.csv(paste0("C:\\Users\\Misha\\Documents\\SandBox\\dfg",
#                             temp_ind,".csv"), stringsAsFactors = FALSE)
#
#   temp_df$date = as.Date(temp_df$date)
#
#   return(temp_df)
#
# })
#
# romain_dates_components = romain_dates_components[
#   order(sapply(romain_dates_components, nrow), decreasing = TRUE)]
#
# romain_time_indices = lapply(romain_dates_components,
#                              function(temp_df){temp_df$date})
#
# romain_time_indices = romain_time_indices[order(sapply(romain_time_indices,
#                                                        length))]
#
# romain_agg_series = lapply(1:6, function(temp_ind){
#
#   temp_df = read.csv(paste0("C:\\Users\\Misha\\Documents\\SandBox\\s_agg",
#                             temp_ind,".csv"),
#                      stringsAsFactors = FALSE, header = FALSE)
#
#   names(temp_df) = c("Date", "PCA")
#
#   temp_df$Date = as.Date(temp_df$Date)
#
#   return(temp_df)
#
# })
#
# romain_agg_series = romain_agg_series[order(sapply(romain_agg_series, nrow))]
#
#
# romain_diff_series = lapply(1:6, function(temp_ind){
#
#   temp_df = read.csv(paste0("C:\\Users\\Misha\\Documents\\SandBox\\s_agg_diff",
#                             temp_ind,".csv"),
#                      stringsAsFactors = FALSE, header = FALSE)
#
#   names(temp_df) = c("Date", "PCA")
#
#   temp_df$Date = as.Date(temp_df$Date)
#
#   return(temp_df[-nrow(temp_df),])
#
# })
#
# romain_diff_series = romain_diff_series[order(sapply(romain_diff_series,
#                                                      nrow))]
#
# romain_chain_df = read.csv("C:\\Users\\Misha\\Documents\\SandBox\\chain.csv",
#                            stringsAsFactors = FALSE, header = FALSE) %>%
#   set_names(c("Date","PCA")) %>%
#   slice(-nrow(.))
#
#
#
# romain_chain_df_retropolated = read.csv(
#   "C:\\Users\\Misha\\Documents\\SandBox\\chain_retropolated.csv",
#                                stringsAsFactors = FALSE, header = FALSE) %>%
#   set_names(c("Date","PCA")) %>%
#   mutate(Date = as.Date(Date)) %>%
#   slice(-nrow(.))
#
#
# romain_domestic_fci = read.csv(
#   paste0("C:\\Users\\Misha\\Documents\\GaR Python",
#          "\\Israel GaR\\Output\\Partitions\\",
#          "partitions_data.csv")) %>%
#   select(date, domestic_fci) %>%
#   mutate(date = as.Date(date)) %>%
#   setNames(c("Date","PCA")) %>%
#   slice(-nrow(.))
#
# # Comparison
#
# for (temp_ind in 1:length(dates_components)) {
#
#   dates_components[[temp_ind]] = dates_components[[temp_ind]] %>%
#     select(names(romain_dates_components[[temp_ind]]))
#
# }
#
# all.equal(time_indices_list, romain_time_indices)
#
# all.equal(dates_components, romain_dates_components)
#
# # Compare PCA components
#
# cor_signs = sapply(1:6, function(temp_ind){
#
#   cor(agg_series[[temp_ind]]$PCA,
#       romain_agg_series[[temp_ind]]$PCA)
#
# })
#
# # Compare PCA components differences
#
# all.equal(cor_signs ,sapply(1:6, function(temp_ind){
#
#   cor(diff_series[[temp_ind]]$PCA,
#       romain_diff_series[[temp_ind]]$PCA)
#
# }))
#
#
#
# chain_df = diff_series[[1]]
#
# chain_df$PCA = chain_df$PCA * cor_signs[1]
#
# for (i in 2:length(diff_series)){
#
#   target_df = diff_series[[i]]
#
#   target_df$PCA = target_df$PCA  * cor_signs[i]
#
#   temp_Date_varname = grep("[Dd]ate", names(target_df),
#                            value = TRUE)
#
#   target_ind = (!target_df[[temp_Date_varname]] %in%
#                   chain_df [[temp_Date_varname]])
#
#   chain_df  = rbind.data.frame(chain_df , target_df[target_ind,])
#
#
# }
#
# chain_df  = chain_df  %>%
#   arrange(Date)
#
#
# chain_df_retropolated = chain_df %>%
#   arrange(desc(Date)) %>%
#   mutate(PCA = cumsum(PCA)) %>%
#   arrange(Date)
#
#
# cor(chain_df$PCA, romain_chain_df$PCA)
#
# cor(chain_df_retropolated$PCA,
#     romain_chain_df_retropolated$PCA)
#
#
