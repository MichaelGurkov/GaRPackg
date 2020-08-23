
context("align pca function")

set.seed(123)

test_data = matrix(data = rnorm(100),ncol = 5)

colnames(test_data) = LETTERS[1:5]

test_pca = prcomp(test_data)

target_pca = test_pca

target_sign_vec = c(1,-1,1,1,-1)

target_pca$x = sapply(1:ncol(target_pca$x),function(temp_ind){
  target_pca$x[,temp_ind] * target_sign_vec[temp_ind]})


target_pca$rotation = sapply(1:ncol(target_pca$rotation),function(temp_ind){
  target_pca$rotation[,temp_ind] * target_sign_vec[temp_ind]})

test_that("PCA alignment by name arg",
          {expect_equal(align_pca(pca_obj = test_pca,var_name = "A"),
                        target_pca)})

test_that("PCA alignment by vec position",
          {expect_equal(align_pca(pca_obj = test_pca,var_name = test_data[,1]),
                        target_pca)})


