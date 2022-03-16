# Tests for the generate family of functions

test_that("random creation works", {
  expect_false(isTRUE(all.equal(generate_random(4,.3), generate_random(4,.3))))
  expect_false(isTRUE(all.equal(generate_random(c(2,4),.3), generate_random(c(2,4),.3))))
  expect_error(generate_random(c(1,2,3)), "must be of length")
  # Bipartite graph
  expect_equal(class(generate_random(ison_southern_women, 0.4)), "igraph")
})

test_that("generate_smallworld() works", {
  expect_equal(class(generate_smallworld(12, 0.025)), "igraph")
})

test_that("generate_scalefree() works", {
  expect_equal(class(generate_scalefree(12, 0.025)), "igraph")
})

test_that("generate_permutation() works", {
  expect_equal(class(generate_permutation(ison_southern_women)), "igraph")
})
