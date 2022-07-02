test_that("graph components works", {
  expect_s3_class(graph_components(mpn_bristol), "graph_measure")
  expect_equal(as.numeric(graph_components(mpn_bristol)), 3)
})

test_that("graph cohesion works", {
  expect_s3_class(graph_cohesion(mpn_bristol), "graph_measure")
  expect_equal(as.numeric(graph_cohesion(mpn_bristol)), 0)
})

test_that("graph adhesion works", {
  expect_s3_class(graph_adhesion(mpn_bristol), "graph_measure")
  expect_equal(as.numeric(graph_adhesion(mpn_bristol)), 0)
})

test_that("graph diameter works", {
  expect_s3_class(graph_diameter(mpn_bristol), "graph_measure")
  expect_equal(as.numeric(graph_diameter(mpn_bristol)), 6)
})

test_that("graph length works", {
  expect_s3_class(graph_length(mpn_bristol), "graph_measure")
  expect_equal(as.numeric(graph_length(mpn_bristol)), 2.451265, 
               tolerance = 0.000001)
})
