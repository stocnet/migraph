test_that("Plot label function works", {
  testplot <- ggraphlabel(adolescent_society)
  expect_true(is.list(testplot))
  expect_length(testplot, 9)
  expect_named(testplot[1], "data")
})
