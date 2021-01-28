#' @title Import data from fame template
#'
#' @description This function imports and cleans data from fame template
#'
#' @details If a complimentary column (a column name with _append suffix) is
#'  supplied, the functions merges the two columns (varname, varname_append)
#'  into one. The merge is performed by substituting NA values in the main
#'  (varname) column with values from the complimentary (varname_append) column.
#'
#'
#' @importFrom  stringr str_subset str_remove
#'
#' @importFrom rlang .data
#'
#' @importFrom lubridate parse_date_time
#'
#' @importFrom zoo as.yearqtr as.yearmon
#'
#' @importFrom rlang :=
#'
#' @importFrom utils read.csv
#'
#' @param template_path file path to template file
#'
#' @param data_frequency string that specifies time frequency of the data.
#'
#' Available options are :
#' \itemize{
#'  \item {quarterly}{ (the default)}
#'  \item {monthly}
#' }
#'
#' @return tidy df
#'
#' @export

import_from_fame_template = function(template_path,
                                     data_frequency = "quarterly") {
  fame_df = read.csv(template_path, stringsAsFactors = FALSE) %>%
    slice(-(1:10)) %>%
    rename(date = 1) %>%
    mutate(date = parse_date_time(.data$date,orders = c("dmy","mdy")))

  if (data_frequency == "quarterly") {

    fame_df = fame_df %>%
      mutate(date = as.yearqtr(.data$date))

  }  else if (data_frequency == "monthly") {

    fame_df = fame_df %>%
      mutate(date = as.yearmon(.data$date))

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
    select(-all_of(append_vars_list)) %>%
    rename_all(tolower)


  return(fame_df)

}

#' This function imports staff forecast df
#'
#' @import readr
#'
#' @import dplyr
#'
#' @importFrom rlang .data
#'



#' @title Import DSGE forecasts
#'
#' @description  This function imports DSGE forecasts and convert the data
#' to tidy format
#'
#' @param file_path
#'
#' @importFrom  readxl read_xlsx
#'
#' @importFrom  zoo as.yearqtr
#'
#' @importFrom tibble tribble
#'
#' @import dplyr
#'
#' @importFrom rlang .data
#'
import_dsge_forecasts = function(file_path){


  cpi_table = tribble(
    ~horizon,~`0.05`,~`0.25`,
    "1",-1.2,-0.5,
    "4",-3.25,-1.3,
    "8",-3.5,-1.45,
    "12",-3.75,-1.5) %>%
    mutate(`0.5` = 0) %>%
    mutate(`0.75` = abs(`0.25`)) %>%
    mutate(`0.95` = abs(`0.05`)) %>%
    mutate(across(-horizon, ~./100))


  gdp_table = tribble(
    ~horizon,~`0.05`,~`0.25`,
    "1",-1.98,-0.8,
    "4",-3.95,-1.6,
    "8",-4.6,-1.8,
    "12",-4.9,-2) %>%
    mutate(`0.5` = 0) %>%
    mutate(`0.75` = abs(`0.25`)) %>%
    mutate(`0.95` = abs(`0.05`)) %>%
    mutate(across(-horizon, ~./100))



  sheet_names_list = list(cpi = "OB_DP_CP_YOY",
                        gdp = "OB_DY_YOY")


  forecast_df = map_dfr(
    sheet_names_list,
    .id = "target_var",
    .f = function(temp_sheet) {
      raw_data = read_xlsx(file_path, sheet = temp_sheet, skip = 1)

      data = raw_data %>%
        rename(date = OBS) %>%
        select(matches("date|[0-9]")) %>%
        mutate(date = as.yearqtr(date)) %>%
        pivot_longer(-"date",
                     names_to = "horizon",
                     values_to = "forecast") %>%
        filter(complete.cases(.))

      data = data %>%
        mutate(date = date - as.numeric(horizon) * 0.25)


      return(data)

    }
  )


  result_df = forecast_df %>%
    filter(target_var == "gdp") %>%
    left_join(gdp_table, by = "horizon") %>%
    filter(complete.cases(.)) %>%
    bind_rows(forecast_df %>%
            filter(target_var == "cpi") %>%
            left_join(cpi_table, by = "horizon") %>%
            filter(complete.cases(.))) %>%
    mutate(across(matches("[0-9]"), ~ . + forecast)) %>%
    select(-forecast) %>%
    pivot_longer(-c("target_var","date", "horizon"),
                 names_to = "quantile",
                 values_to = "forecast")

  return(result_df)

}
