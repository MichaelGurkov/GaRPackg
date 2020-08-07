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

get_partition_combs = function(optional_vars_vec,
                               required_vars_vec = NULL,
                               partition_name = NULL){

  comb_df = map(seq_along(optional_vars_vec),
                  function(temp_ind){

    comb_list =  combn(optional_vars_vec,temp_ind,simplify = FALSE)

    if(!is.null(required_vars_vec)){


      comb_list = map(comb_list, function(temp_part){

        temp_part = c(temp_part, required_vars_vec)

      })


    }

    comb_list = map(comb_list, function(temp_vec){

      temp_list = list(temp_vec)

      names(temp_list) = partition_name

      return(temp_list)


    })

    names(comb_list) = rep(partition_name, length(comb_list))

    temp_comb_df = comb_list %>%
      enframe %>%
      mutate(name = paste(temp_ind,name, sep = "-"))


                  }) %>%
    bind_rows()

  if(!is.null(partition_name)){

    comb_df = comb_df %>%
      rename(!!sym(partition_name) := value)

  }

  return(comb_df)



}
