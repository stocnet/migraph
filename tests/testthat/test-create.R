test_that("lattice creation works", {
  expect_equal(create_chain(2,4, as = "matrix"), matrix(1,2,4))
  expect_equivalent(create_chain(2,4, as = "igraph"), igraph::graph_from_incidence_matrix(matrix(1,2,4)))
  expect_equivalent(create_chain(2,4, as = "tbl_graph"), tidygraph::as_tbl_graph(matrix(1,2,4)))
})

test_that("silo creation works", {
  expect_equal(create_silos(2,4, as = "matrix"), matrix(c(1,0,1,0,0,1,0,1),2,4))
  expect_s3_class(create_silos(2,4, as = "igraph"), "igraph")
  expect_s3_class(create_silos(2,4, as = "tbl_graph"), "tbl_graph")
})

test_that("match creation works", {
  expect_equal(create_match(2,4, as = "matrix"), matrix(c(1,0,0,1,1,0,0,1),2,4))
  expect_s3_class(create_match(2,4, as = "igraph"), "igraph")
  expect_s3_class(create_match(2,4, as = "tbl_graph"), "tbl_graph")
})

test_that("nest creation works", {
  expect_equal(create_nest(2,4, as = "matrix"), matrix(c(1,1,0,1,0,0,0,0),2,4))
  expect_s3_class(create_nest(2,4, as = "igraph"), "igraph")
  expect_s3_class(create_nest(2,4, as = "tbl_graph"), "tbl_graph")
})

test_that("random creation works", {
  expect_false(isTRUE(all.equal(play_twomode(2,4,.3), play_twomode(2,4,.3))))
})

test_that("star creation works", {
  expect_equal(create_star(1,4, as = "matrix"), matrix(c(1,1,1,1),1,4))
  expect_equivalent(create_star(1,4, as = "igraph"), igraph::graph_from_incidence_matrix(matrix(1,1,4)))
  expect_equivalent(create_star(1,4, as = "tbl_graph"), tidygraph::as_tbl_graph(matrix(1,1,4)))
})

