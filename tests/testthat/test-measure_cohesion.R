test_that("graph density works", {
  expect_s3_class(graph_density(ison_southern_women), "graph_measure")
  expect_equal(as.numeric(graph_density(create_empty(10))), 0)
  expect_equal(as.numeric(graph_density(create_empty(c(10,6)))), 0)
  expect_equal(as.numeric(graph_density(create_complete(10))), 1)
  expect_equal(as.numeric(graph_density(create_complete(c(10,6)))), 1)
  expect_output(print(graph_density(create_complete(10))))
})

test_that("graph reciprocity works", {
  expect_s3_class(graph_reciprocity(ison_networkers), "graph_measure")
  expect_output(print(graph_reciprocity(ison_networkers)))
  expect_length(graph_reciprocity(ison_networkers), 1)
  expect_equal(as.numeric(graph_reciprocity(ison_networkers)),
               igraph::reciprocity(as_igraph(ison_networkers)))
})

test_that("one-mode object clustering is reported correctly",{
  expect_equal(as.numeric(graph_transitivity(ison_algebra)),
               0.69787, tolerance = 0.001)
  expect_s3_class(graph_transitivity(ison_algebra), "graph_measure")
  expect_output(print(graph_transitivity(ison_algebra)))
})

test_that("two-mode object clustering is reported correctly",{
  expect_equal(as.numeric(graph_equivalency(ison_southern_women)),
               0.4872, tolerance = 0.001)
  expect_s3_class(graph_equivalency(ison_southern_women), "graph_measure")
  expect_output(print(graph_equivalency(ison_southern_women)))
})

test_that("three-mode clustering calculated correctly",{
  mat1 <- create_ring(5,10)
  mat2 <- create_ring(5,8)
  expect_equal(as.numeric(graph_congruency(mat1, mat2)),
               0.7143, tolerance = 0.001)
  expect_s3_class(graph_congruency(mat1, mat2), "graph_measure")
  expect_output(print(graph_congruency(mat1, mat2)))
})

