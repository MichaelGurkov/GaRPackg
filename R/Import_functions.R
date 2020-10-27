#' This function imports clean data from fame template
#'
#' @title Returns tidy df
#'
#' @import stringr
#'
#' @return df

import.from.fame.template = function(template_path) {
  fame_df = read.csv(template_path, stringsAsFactors = FALSE) %>%
    slice(-(1:10)) %>%
    rename(date = 1)

  if (!is.na(as.yearqtr(fame_df$date[1], format = "%d/%m/%Y"))) {
    fame_df = fame_df %>%
      mutate(date = as.yearqtr(date, format = "%d/%m/%Y"))

  }
  else if (!is.na(as.yearqtr(fame_df$date[1], format = "%m/%d/%Y"))) {
    fame_df = fame_df %>%
      mutate(date = as.yearqtr(date, format = "%m/%d/%Y"))

  }


  # Substitute empty strings with NA

  fame_df = fame_df %>%
    mutate_all( ~ na_if(., "")) %>%
    mutate_if(is.character, .funs = list( ~ as.numeric(.)))

  # Append missing series

  append_vars_list = str_subset(names(fame_df), "_append$")

  for (append_var in append_vars_list) {
    target_var = str_remove(append_var, "_append$")

    fame_df = fame_df %>%
      mutate(!!target_var := coalesce(!!sym(append_var), !!sym(target_var)))

    rm(target_var)


  }

  fame_df = fame_df %>%
    select(-all_of(append_vars_list))


  return(fame_df)

}

#' This function imports staff forecast df
#'
#' @import readr
#'
#' @import dplyr
#'
import.staff.forecast = function(raw_df,
                                 conversion_table_path = NULL) {
  if (is.null(conversion_table_path)) {
    conversion_table_path = paste0(
      file.path(Sys.getenv("USERPROFILE"), fsep = "\\"),
      "\\OneDrive - Bank Of Israel\\Data\\BoI\\GaR_Data\\",
      "Staff_forecast_conversion_table.csv"
    )

  }

  staff_forecast_conversion_table = read_csv(conversion_table_path)

  staff_forecast_conversion_table = staff_forecast_conversion_table %>%
    filter(Type == "YoY") %>%
    select(-Type) %>%
    mutate(Horizon = as_factor(Horizon, )) %>%
    pivot_longer(cols = c(-Horizon),
                 names_to = "Quantile",
                 values_to = "Conversion_Coeff") %>%
    mutate(Conversion_Coeff = Conversion_Coeff * 0.01)

  staff_forecast_conversion_table = full_join(staff_forecast_conversion_table,
                                              select(raw_df, date),
                                              by = character())


  staff_forecast_df_h1 = raw_df %>%
    select(date, staff_1q) %>%
    rename_all(.funs = list( ~ str_remove(., "staff_"))) %>%
    gather(key = Quarter, value = Forecast,-date) %>%
    filter(complete.cases(.)) %>%
    left_join(., raw_df %>%
                select(date, gdp) %>%
                mutate(gdp = gdp / lag(gdp, 3) - 1)) %>%
    mutate(Staff_Forecast = Forecast * 0.25 * 0.01 + gdp) %>%
    select(date, Staff_Forecast) %>%
    mutate(Forecast_Period = paste(date - 0.75, date + 0.25,
                                   sep = "-")) %>%
    mutate(Horizon = "1")


  staff_forecast_df_h4 = raw_df %>%
    select(date, staff_1q, staff_2q, staff_3q, staff_4q) %>%
    rename_all(.funs = list( ~ str_remove(., "staff_"))) %>%
    gather(key = Quarter, value = Forecast,-date) %>%
    group_by(date) %>%
    summarise(Staff_Forecast = mean(Forecast) * 0.01) %>%
    ungroup() %>%
    filter(complete.cases(.)) %>%
    mutate(Forecast_Period = paste(date, (date + 1),
                                   sep = "-")) %>%
    mutate(Horizon = "4")



  staff_forecast_df_h8 = raw_df %>%
    select(date, staff_5q, staff_6q, staff_7q, staff_8q) %>%
    rename_all(.funs = list( ~ str_remove(., "staff_"))) %>%
    gather(key = Quarter, value = Forecast,-date) %>%
    group_by(date) %>%
    summarise(Staff_Forecast = mean(Forecast) * 0.01) %>%
    ungroup() %>%
    filter(complete.cases(.)) %>%
    mutate(Forecast_Period = paste((date + 1), (date + 2),
                                   sep = "-")) %>%
    mutate(Horizon = "8")

  staff_forecast = list(staff_forecast_df_h1,
                        staff_forecast_df_h4,
                        staff_forecast_df_h8) %>%
    bind_rows() %>%
    mutate(date = as.yearqtr(date)) %>%
    inner_join(., staff_forecast_conversion_table,
               by = c("date", "Horizon")) %>%
    mutate(Staff_Forecast = Staff_Forecast + Conversion_Coeff) %>%
    mutate(Quantile = str_replace_all(Quantile, "0.5", "0.50")) %>%
    select(-Conversion_Coeff) %>%
    select(date, Horizon, Quantile, Forecast_Period, Staff_Forecast)


  return(staff_forecast)

}
