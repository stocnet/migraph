test_that("small-world metrics for two mode networks are calculated and displayed correctly", {
  expect_equal(graph_smallworld(ison_southern_women), 1.313936, tolerance = 0.01)
})
