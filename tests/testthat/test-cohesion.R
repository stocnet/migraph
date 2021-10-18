test_that("density works", {
  expect_equal(graph_density(create_empty(10)), 0)
  expect_equal(graph_density(create_empty(c(10,6))), 0)
  expect_equal(graph_density(create_complete(10)), 1)
  expect_equal(graph_density(create_complete(c(10,6))), 1)
})

test_that("one-mode object clustering is reported correctly",{
  expect_equal(round(graph_transitivity(ison_m182), 2), 0.7)
})

test_that("two-mode object clustering is reported correctly",{
  expect_equal(round(graph_equivalency(southern_women), 4), 0.4872)
})

mat1 <- create_ring(5,10)
mat2 <- create_ring(5,8)

test_that("three-mode clustering calculated correctly",{
  expect_equal(round(graph_equivalency(mat1, mat2), 4), 0.7143)
})

