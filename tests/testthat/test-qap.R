mat1 <- matrix(c(0,1,1,0,0,1,1,1),4,2)
mat2 <- matrix(c(0,1,0,1,0,1,0,1),4,2)
mat3 <- matrix(c(0,0,1,1,0,0,1,1),4,2)

test_that("coefficients calculated correctly",{
  expect_equal(netlm2(mat1, IV = list(mat2, mat3))$results$Coefficients, c(0.37, 0.25, 0.25), tolerance = .011)
})

test_that("p-values calculated correctly",{
    expect_equal(netlm2(mat1, IV = list(mat2, mat3))$results$Pvalue, c(0.38, 0.84, 0.79), tolerance = .02)
})