mat1 <- matrix(c(0,1,1,0,0,1,1,1),4,2)
mat2 <- matrix(c(0,1,0,1,0,1,0,1),4,2)
mat3 <- matrix(c(0,0,1,1,0,0,1,1),4,2)
lmat <- list(mat1 = mat1, mat2 = mat2, mat3 = mat3)

test_that("netlm expects correctly",{
  expect_error(netlm(mat1 ~ mat2 + mat3, mat1), "expects a list")
  expect_error(netlm(mat1 ~ mat2 + mat3, lapply(lmat, function(x) as.data.frame(x))), "expects a list of matrices")
})

test_that("netlm estimates correctly",{
  set.seed(123)
  test <- netlm(mat1 ~ mat2 + mat3, lmat)
  expect_s3_class(test, "netlm")
  expect_equal(unname(test$coefficients), c(0.375, 0.250, 0.250))
})

test_that("summary and print work correctly for netlm",{
  set.seed(123)
  expect_error(summary.netlm(lmat), "expects an object of class 'netlm'")
  test <- summary(netlm(mat1 ~ mat2 + mat3, lmat))
  expect_s3_class(test, "summary.netlm")
  expect_equal(test$r.squared, 0.1333333, tolerance = 0.01)
  expect_equal(test$adj.r.squared, -0.2133333, tolerance = 0.01)
  # expect_equal(test$pvals, c(0.38, 0.84, 0.79), tolerance = 0.05)
})
