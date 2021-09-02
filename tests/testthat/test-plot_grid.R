test_that("Plot grid function works", {
  testplot <- plot_grid(mpn_bristol)
  expect_true(is.list(testplot))
  expect_length(testplot, 9)
  expect_named(testplot[1], "data")
})
