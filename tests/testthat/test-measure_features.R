test_that("small-world metrics for two mode networks are calculated and displayed correctly", {
  expect_s3_class(graph_smallworld(ison_southern_women), "graph_measure")
  expect_equal(as.numeric(graph_smallworld(ison_southern_women)), 1.313936, tolerance = 0.01)
})
