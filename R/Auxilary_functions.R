#' This function returns a data_frame with list-column
#' partition combs
#'
#' @param optional_vars_vec
#'
#' @param required_vars_vec
#'
#' @param partition_name
#'
#' @import purrr
#'
#' @import tibble
#'

get_partition_combs = function(partition_list,
                               partition_name){

  if(all(length(names(partition_list)) == 1 &
         names(partition_list) == "required")){

    temp_comb_df = partition_list %>%
      enframe %>%
      mutate(name = paste(partition_name,1, sep = "-")) %>%
      rename(!!sym(partition_name) := value)

    return(temp_comb_df)
  }

  comb_df = map(seq_along(partition_list$optional),
                  function(temp_ind){

    comb_list =  combn(partition_list$optional,temp_ind,simplify = FALSE)

    temp_comb_df = comb_list %>%
      enframe %>%
      mutate(name = paste(partition_name,temp_ind, sep = "-"))


                  }) %>%
    bind_rows()

  if("required" %in% names(partition_list)){

    comb_df = comb_df %>%
      mutate(value = map(value, ~ c(.,partition_list$required)))


  }


  comb_df = comb_df %>%
    mutate(value = map(value, function(temp_vec){

    temp_list = list(temp_vec)

    names(temp_list) = partition_name

    return(temp_list)


  }))

  comb_df = comb_df %>%
    rename(!!sym(partition_name) := value)

  return(comb_df)



}
