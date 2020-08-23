part_list =  list(
  required = c(
    "Spread_CPI_corp",
    "BOI_rate"),
  optional = c(
    "Sovereigh_spread",
    "Term_spread")
  )

part_name = "Dom_FCI"

test_required_object = get_partition_combs(partition_list = part_list[1],
                           partition_name = part_name)

expected_required_object = part_list[1] %>%
  enframe() %>%
  mutate(value = map(value, function(temp_vec){

    temp_list = list(temp_vec)

    names(temp_list) = part_name

    return(temp_list)


  })) %>%
  rename(!!sym(part_name) := value) %>%
  mutate(name = paste(part_name,1,sep = "-"))


test_that("get_partition_combs handles required only list",
          expect_equal(object = test_required_object,
                       expected = expected_required_object))


test_optional_object = get_partition_combs(partition_list = part_list[2],
                                           partition_name = part_name)

expected_optional_object = list(
  Dom_FCI = "Sovereigh_spread",
  Dom_FCI = "Term_spread",
  Dom_FCI = c("Sovereigh_spread","Term_spread"),
  Dom_FCI = "") %>%
  enframe() %>%
  mutate(name = paste(name, c(1,1,2,0), sep = "-")) %>%
  mutate(value = map(value, function(temp_vec){

    temp_list = list(temp_vec)

    names(temp_list) = part_name

    return(temp_list)


  })) %>%
  rename(!!sym(part_name) := value)


test_that("get_partition_combs handles optional only list",
          expect_equal(object = test_optional_object,
                       expected = expected_optional_object))
