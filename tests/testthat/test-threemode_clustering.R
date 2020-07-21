mat1 <- matrix(0,3,3)
mat1[1,1:2] <- 1
mat1[2,2] <- 1
mat2 <- matrix(0,3,3)
mat2[1:2,1] <- 1
mat2[3,3] <- 1

test_that("three-mode clustering calculated correctly",{
  expect_equal(threemode_clustering(mat1, mat2), 2/3)
})