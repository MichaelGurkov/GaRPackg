#' @title Return combinations of partition elements
#'
#' @description  This function returns a data_frame with list-column
#' partition combs
#'
#' @import purrr
#'
#' @import tibble
#'
#' @importFrom rlang .data
#'
#' @importFrom rlang :=
#'
#' @importFrom utils combn
#'
#' @param partitions_list a named list where the names are
#' \itemize{
#'  \item{"optional"}{-Optional elements}
#'  \item{"required"}{-Required elements}
#'  }
#'
#' @param partition_name name of the partition for resulting df
#'
#'

get_partition_combs = function(partitions_list,
                               partition_name) {

  # if only required category present return as data frame

  if (all(length(names(partitions_list)) == 1 &
          names(partitions_list) == "required")) {

    temp_comb_df = partitions_list %>%
      tibble::enframe() %>%
      dplyr::mutate(value = purrr::map(.data$value, function(temp_vec) {
        temp_list = list(temp_vec)

        names(temp_list) = partition_name

        return(temp_list)

      })) %>%
      dplyr::rename(!!sym(partition_name) := .data$value) %>%
      dplyr::mutate(name = paste(partition_name, 1, sep = "-"))


    return(temp_comb_df)
  }

  # make all combinations of optional category --> comb_df

  comb_df = purrr::map(seq_along(partitions_list$optional),
                function(temp_ind) {
                  comb_list =  utils::combn(partitions_list$optional,
                                            temp_ind, simplify = FALSE)

                  temp_comb_df = comb_list %>%
                    tibble::enframe() %>%
                    dplyr::mutate(name = paste(partition_name, temp_ind, sep = "-"))
                }) %>%
    dplyr::bind_rows() %>%
    rbind(data.frame(name = paste0(partition_name, "-0"), value = ""))

  # if required category present add to each combination in comb_df

  if ("required" %in% names(partitions_list)) {
    comb_df = comb_df %>%
      dplyr::mutate(value = purrr::map(.data$value, ~ c(., partitions_list$required)))
  }


  comb_df = comb_df %>%
    dplyr::mutate(value = purrr::map(.data$value, function(temp_vec) {
      temp_list = list(temp_vec)

      names(temp_list) = partition_name

      return(temp_list)

    }))

  comb_df = comb_df %>%
    dplyr::rename(!!sym(partition_name) := .data$value)

  return(comb_df)

}

#' @description  This function compares two partitions
#'
#' @title compare two partitions
#'
#' @param source_partition benchmark partition
#'
#' @param target_partition compared partition
#'

is_partition_identical = function(source_partition, target_partition){

  names_diff = union(
    setdiff(names(source_partition),names(target_partition)),
    setdiff(names(target_partition),names(source_partition)))

  if(!length(names_diff) == 0){return(FALSE)}

  target_partition = target_partition[names(source_partition)]

  for(temp_name in names(source_partition)){

    if(!length(source_partition[[temp_name]]) ==
       length(target_partition[[temp_name]])){return(FALSE)}

    comp_diff = union(
      setdiff(source_partition[[temp_name]],target_partition[[temp_name]]),
      setdiff(target_partition[[temp_name]],source_partition[[temp_name]]))


    if(!length(comp_diff) == 0){return(FALSE)}


  }

  return(TRUE)


}



#' @description  This function identifies the time frequency of a
#'  date column in a data frame
#'
#' @title Identify time frequency of date column
#'
#' @importFrom dplyr case_when
#'
#'
#' @param date_col
#'
identify_frequency = function(date_col){

  time_frequency = case_when(
    class(date_col) == "yearmon" ~ "monthly",
    class(date_col) == "yearqtr" ~ "quarterly",
    all(!is.na(as.yearmon(date_col))) ~ "monthly",
    all(!is.na(as.yearqtr(date_col))) ~ "quarterly"
  )

  return(time_frequency)


}


#' @description This function determines the required \code{win_len}
#'  for the analysis set so that the evaluation set will starts at
#'  the specified date
#'
#' @title Calculate window length for start date
#'
#'  @param start_date
#'
#'  @param data_df
#'
calculate_win_len_from_date = function(start_date,data_df){

  date_vec = data_df$date

  win_len = max(which(start_date >= date_vec))

  if(length(win_len) > 0){

    return(win_len)

  }

  else {stop(paste0("window length could not be set.",
                    " Perhaps start date is out of data",
                    " date range"))}


}
