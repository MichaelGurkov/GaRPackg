data("gar_data")

test_params = list()

test_params$partition = list(
  Dom_Macro = list("GDP","Ind_Prod_Israel"),
  FinCycle = list("Credit","House_Price"))

test_params$horizon_list = list(1,4)

test_params$quantile_vec = c(0.05,0.5)

temp = gar_data %>%
  reduce_data_dimension(partition = test_params$partition)


# Year on year
#-----------------------------------------------------------------------------------------
result_df_yoy = gar_data%>%
  select(Date, GDP) %>%
  mutate(GDP_1 = lead(GDP,1),GDP_4 = lead(GDP,4)) %>%
  inner_join(temp$xreg_df, by = c("Date" = "date"))


test_df_yoy = make.quant.reg.df(
  partitions_list = test_params$partition,
  vars_df = gar_data,
  target_var_name = "GDP",
  horizon_list = test_params$horizon_list,
  quantile_vec = test_params$quantile_vec)[[1]]


test_that("make.quant.reg.df produces year on year df",
          code = expect_equal(
            object = test_df_yoy %>%
              select(names(result_df_yoy)),
            expected = result_df_yoy
            )
          )




# Cumulative
#-----------------------------------------------------------------------------------------
result_df_cum = gar_data%>%
  select(Date, GDP) %>%
  mutate(GDP_1 = (lead(GDP,1) / GDP) - 1,GDP_4 = (lead(GDP,4) / GDP) - 1) %>%
  inner_join(temp$xreg_df, by = c("Date" = "date"))


result_df_cum_null = gar_data%>%
  select(Date, GDP) %>%
  mutate(GDP_1 = (lead(GDP,1) / GDP) - 1,GDP_4 = (lead(GDP,4) / GDP) - 1) %>%
  inner_join(temp$xreg_df, by = c("Date" = "date"))


test_df_cum = make.quant.reg.df(
  partitions_list = test_params$partition,
  vars_df = gar_data,
  target_var_name = "GDP",
  horizon_list = test_params$horizon_list,
  quantile_vec = test_params$quantile_vec,
  type = "Cumulative")[[1]]

test_df_cum_null = make.quant.reg.df(
  partitions_list = NULL,
  vars_df = gar_data,
  target_var_name = "GDP",
  horizon_list = test_params$horizon_list,
  quantile_vec = test_params$quantile_vec,
  type = "Cumulative")[[1]]

#-----------------------------------------------------------------------------------------


test_that("make.quant.reg.df produces cumulative df",
          code = expect_equal(
            object = test_df_cum %>%
              select(names(result_df_cum)),
            expected = result_df_cum
          )
)

test_that("make.quant.reg.df produces cumulative df with partition NULL",
          code = expect_equal(
            object = test_df_cum_null,
            expected = )
          )
)
