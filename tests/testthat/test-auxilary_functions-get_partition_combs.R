part_list =  list(
  required = c(
    "spread_cpi_corp",
    "boi_rate"),
  optional = c(
    "sovereigh_spread",
    "term_spread")
  )

part_name = "dom_fci"

test_required_object = get_partition_combs(partitions_list = part_list[1],
                           partition_name = part_name)

expected_required_object = part_list[1] %>%
  enframe() %>%
  dplyr::mutate(value = purrr::map(value, function(temp_vec){

    temp_list = list(temp_vec)

    names(temp_list) = part_name

    return(temp_list)


  })) %>%
  dplyr::rename(!!sym(part_name) := value) %>%
  dplyr::mutate(name = paste(part_name,1,sep = "-"))


test_that("get_partition_combs handles required only list",
          expect_equal(object = test_required_object,
                       expected = expected_required_object))


test_optional_object = get_partition_combs(partitions_list = part_list[2],
                                           partition_name = part_name)

expected_optional_object = list(
  dom_fci = "sovereigh_spread",
  dom_fci = "term_spread",
  dom_fci = c("sovereigh_spread","term_spread"),
  dom_fci = "") %>%
  enframe() %>%
  dplyr::mutate(name = paste(name, c(1,1,2,0), sep = "-")) %>%
  dplyr::mutate(value = purrr::map(value, function(temp_vec){

    temp_list = list(temp_vec)

    names(temp_list) = part_name

    return(temp_list)


  })) %>%
  dplyr::rename(!!sym(part_name) := value)


test_that("get_partition_combs handles optional only list",
          expect_equal(object = test_optional_object,
                       expected = expected_optional_object))
