

test_that("identical partitions return TRUE", {
  expect_true(
    is_partition_identical(
      source_partition = list(A = c("a","b"), B = c("c","d")),
      target_partition = list(B = c("c","d"),A = c("a","b"))))
})


test_that("different partitions return FALSE", {
  expect_false(
    is_partition_identical(
      source_partition = list(A = c("a","b"), B = c("c","d")),
      target_partition = list(B = c("c","d"),A = c("a","b"), C = c("c","d"))))
})
