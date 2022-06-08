test_that("density works", {
  expect_equal(as.numeric(graph_density(create_empty(10))), 0)
  expect_equal(as.numeric(graph_density(create_empty(c(10,6)))), 0)
  expect_equal(as.numeric(graph_density(create_complete(10))), 1)
  expect_equal(as.numeric(graph_density(create_complete(c(10,6)))), 1)
})

test_that("one-mode object clustering is reported correctly",{
  expect_equal(as.numeric(graph_transitivity(ison_algebra)), 0.69787, tolerance = 0.001)
})

test_that("two-mode object clustering is reported correctly",{
  expect_equal(as.numeric(graph_equivalency(ison_southern_women)), 0.4872, tolerance = 0.001)
})

test_that("three-mode clustering calculated correctly",{
  mat1 <- create_ring(5,10)
  mat2 <- create_ring(5,8)
  expect_equal(as.numeric(graph_congruency(mat1, mat2)), 0.7143, tolerance = 0.001)
})

