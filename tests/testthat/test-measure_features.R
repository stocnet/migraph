test_that("small-world metrics for two mode networks are calculated and displayed correctly", {
  expect_s3_class(graph_smallworld(ison_southern_women), "graph_measure")
  expect_equal(as.numeric(graph_smallworld(ison_southern_women)), 1.313936, tolerance = 0.01)
})

test_that("graph balance works", {
  expect_equal(class(graph_balance(ison_marvel_relationships)),
               "numeric")
  expect_length(graph_balance(ison_marvel_relationships), 1)
  expect_error(graph_balance(ison_adolescents))
})

test_that("graph modularity works for two mode networks", {
  expect_length(graph_modularity(ison_southern_women,
                                 node_kernighanlin(ison_southern_women)), 1)
  expect_equal(class(graph_modularity(ison_southern_women,
                                node_kernighanlin(ison_southern_women))),
               "numeric")
})
