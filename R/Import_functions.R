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

  where = NULL

  fame_df = read.csv(template_path, stringsAsFactors = FALSE) %>%
    dplyr::slice(-(1:10)) %>%
    dplyr::rename(date = 1) %>%
    dplyr::mutate(date = parse_date_time(date,orders = c("dmy","mdy")))

  if (data_frequency == "quarterly") {

    fame_df = fame_df %>%
      dplyr::mutate(date = as.yearqtr(date))

  }  else if (data_frequency == "monthly") {

    fame_df = fame_df %>%
      dplyr::mutate(date = as.yearmon(date))

  }




  # Substitute empty strings with NA

  fame_df = fame_df %>%
    dplyr::mutate(across(everything(),~ na_if(., ""))) %>%
    dplyr::mutate(across(where(is.character),~stringr::str_remove_all(.,","))) %>%
    dplyr::mutate(across(where(is.character), as.numeric))


  # Append missing series

  append_vars_list = stringr::str_subset(names(fame_df), "_append$")

  for (append_var in append_vars_list) {
    target_var = str_remove(append_var, "_append$")

    fame_df = fame_df %>%
      dplyr::mutate(!!target_var := dplyr::coalesce(!!sym(append_var), !!sym(target_var)))

    rm(target_var)


  }

  fame_df = fame_df %>%
    dplyr::select(-dplyr::all_of(append_vars_list)) %>%
    dplyr::rename_all(tolower)


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
#' to tidy format.
#'
#' @param file_path string
#'
#' @param quantiles numeric vector that specifies the required
#'  quantiles for the DSGE forecast
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
#' @export
#'
import_dsge_forecasts = function(file_path,
                                 quantiles = c(0.05,0.25,0.5,0.75,0.95)){

  . = NULL


  cpi_table = tibble::tribble(
    ~horizon,~`0.05`,~`0.25`,
    "1",-1.2,-0.5,
    "4",-3.25,-1.3,
    "8",-3.5,-1.45,
    "12",-3.75,-1.5) %>%
    dplyr::mutate(across(-horizon, ~./100))


  gdp_table = tibble::tribble(
    ~horizon,~`0.05`,~`0.25`,
    "1",-1.98,-0.8,
    "4",-3.95,-1.6,
    "8",-4.6,-1.8,
    "12",-4.9,-2) %>%
    dplyr::mutate(across(-horizon, ~./100))

  sigma_df = cpi_table %>%
    tidyr::pivot_longer(-horizon,names_to = "quantile") %>%
    dplyr::mutate(target_var = "cpi") %>%
    dplyr::bind_rows(gdp_table %>%
                pivot_longer(-horizon,names_to = "quantile") %>%
                  dplyr::mutate(target_var = "gdp")) %>%
    dplyr::mutate(quantile = as.numeric(quantile)) %>%
    dplyr::group_by(horizon, target_var) %>%
    dplyr::summarise(sigma = calculate_implied_std_dev(value, quantile),
              .groups = "drop")



  sheet_names_list = list(cpi = "OB_DP_CP_YOY",
                        gdp = "OB_DY_YOY")


  forecast_df = purrr::map_dfr(
    sheet_names_list,
    .id = "target_var",
    .f = function(temp_sheet) {
      raw_data = readxl::read_xlsx(file_path, sheet = temp_sheet,
                                   skip = 1)

      data = raw_data %>%
        dplyr::rename(date = OBS) %>%
        dplyr::select(matches("date|[0-9]")) %>%
        dplyr::mutate(date = as.yearqtr(date)) %>%
        tidyr::pivot_longer(-"date",
                     names_to = "horizon",
                     values_to = "forecast") %>%
        dplyr::filter(complete.cases(.))

      return(data)

    }
  )





  result_df = sigma_df %>%
    mutate(temp_df = purrr::map(sigma, function(temp_sigma){

      temp_tibble = tibble::tibble(quantiles = quantiles) %>%
        dplyr::mutate(shift = qnorm(quantiles,sd = temp_sigma))


    })) %>%
    dplyr::select(-sigma) %>%
    tidyr::unnest(temp_df) %>%
    tidyr::pivot_wider(names_from = "quantiles",values_from = "shift") %>%
    dplyr::inner_join(forecast_df,by = c("horizon", "target_var")) %>%
    dplyr::mutate(across(matches("[0-9]"), ~ . + forecast)) %>%
    dplyr::select(-forecast) %>%
    tidyr::pivot_longer(-c("target_var","date", "horizon"),
                        names_to = "quantile",
                        values_to = "forecast") %>%
    dplyr::mutate(quantile = as.character(quantile)) %>%
    dplyr::mutate(quantile = dplyr::recode(quantile, "0.5" = "0.50"))

  return(result_df)

}
