test_that("Plot grid function works", {
  testplot <- ggraphgrid(mpn_elite_mex)
  expect_true(is.list(testplot))
  expect_length(testplot, 9)
  expect_named(testplot[1], "data")
})
