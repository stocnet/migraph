test_that("density works", {
  expect_equal(graph_density(create_empty(10)), 0)
  expect_equal(graph_density(create_empty(c(10,6))), 0)
  expect_equal(graph_density(create_complete(10)), 1)
  expect_equal(graph_density(create_complete(c(10,6))), 1)
})
