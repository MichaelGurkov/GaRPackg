#' @details This function imports clean data from fame template
#'
#' @title Returns tidy df
#'
#' @importFrom  stringr str_subset str_remove
#'
#' @importFrom rlang .data
#'
#' @importFrom rlang :=
#'
#' @importFrom utils read.csv
#'
#' @param template_path file path to template file
#'
#' @return df
#'
#' @export

import_from_fame_template = function(template_path) {
  fame_df = read.csv(template_path, stringsAsFactors = FALSE) %>%
    slice(-(1:10)) %>%
    rename(date = 1)

  if (!is.na(as.yearqtr(fame_df$date[1], format = "%d/%m/%Y"))) {
    fame_df = fame_df %>%
      mutate(date = as.yearqtr(.data$date, format = "%d/%m/%Y"))

  }
  else if (!is.na(as.yearqtr(fame_df$date[1], format = "%m/%d/%Y"))) {
    fame_df = fame_df %>%
      mutate(date = as.yearqtr(.data$date, format = "%m/%d/%Y"))

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
