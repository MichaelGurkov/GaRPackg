part_list =  list(
  required = c(
    "Spread_CPI_corp",
    "Sovereigh_spread",
    "Term_spread",
    "ILS_USD_impl_vol",
    "TA_35_impl_vol",
    "BOI_rate")
  )

part_name = "Dom_FCI"

test_object = get_partition_combs(partition_list = part_list,
                           partition_name = part_name)

expected_object = part_list %>%
  enframe() %>%
  mutate(value = map(value, function(temp_vec){

    temp_list = list(temp_vec)

    names(temp_list) = part_name

    return(temp_list)


  })) %>%
  rename(!!sym(part_name) := value) %>%
  mutate(name = paste(part_name,1,sep = "-"))


test_that("get_partition_combs handles required only list",
          expect_equal(object = test_object,
                       expected = expected_object))
