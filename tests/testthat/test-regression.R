test <- network_reg(weight ~ ego(Citations) + alter(Citations) + same(Discipline), ison_eies)

test_that("network_reg estimates correctly",{
  set.seed(123)
  expect_s3_class(test, "netlm")
  expect_equal(round(unname(test$coefficients),3), c(19.506, -0.150, -0.127, 4.194))
})

test <- summary(test, reps = 100)

test_that("summary and print work correctly for network_reg",{
  set.seed(123)
  expect_s3_class(test, "summary.netlm")
  expect_equal(test$r.squared, 0.0223, tolerance = 0.01)
  expect_equal(test$adj.r.squared, 0.0194, tolerance = 0.01)
  # expect_equal(test$pvals, c(0.38, 0.84, 0.79), tolerance = 0.05)
})

test_that("print method work correctly for netlm",{
  set.seed(123)
  test <- capture.output(print(test))
  expect_equal(test[[2]], "Call:")
  expect_equal(test[[3]], "network_reg(formula = weight ~ ego(Citations) + alter(Citations) + ")
  expect_equal(test[[7]], "Coefficients:")
  expect_equal(test[[13]], "---")
})
