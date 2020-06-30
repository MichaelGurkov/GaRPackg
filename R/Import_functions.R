#' This function imports clean data from fame template
#'

#' @title Returns tidy df
#'
#' @import stringr
#'
#' @return df

import.from.fame.template = function(template_path){

  fame_df = read.csv(template_path,stringsAsFactors = FALSE) %>%
    slice(-(1:10)) %>%
    rename(Date = 1)

  if(!is.na(as.yearqtr(fame_df$Date[1], format = "%d/%m/%Y"))){

    fame_df = fame_df %>%
      mutate(Date = as.yearqtr(Date, format = "%d/%m/%Y"))

  }
  else if(!is.na(as.yearqtr(fame_df$Date[1], format = "%m/%d/%Y"))){

    fame_df = fame_df %>%
      mutate(Date = as.yearqtr(Date, format = "%m/%d/%Y"))

  }


  # Substitute empty strings with NA

  fame_df = fame_df %>%
    mutate_all(~na_if(.,"")) %>%
    mutate_if(is.character,.funs = list(~as.numeric(.)))

  # Append missing series

  append_vars_list = str_subset(names(fame_df),"_append$")

  for (append_var in append_vars_list){

    target_var = str_remove(append_var,"_append$")

    fame_df = fame_df %>%
      mutate(!!target_var := coalesce(!!sym(append_var),!!sym(target_var)))

    rm(target_var)


  }

  fame_df = fame_df %>%
    select(-all_of(append_vars_list))


  return(fame_df)

}
