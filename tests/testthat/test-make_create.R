test_that("create empty graph works", {
  expect_true(is_twomode(create_empty(c(5,5))))
  expect_s3_class(create_empty(4), "igraph")
  expect_error(create_empty(c(5,5,5)), "single integer")
})

test_that("create complete graph works", {
  expect_true(is_twomode(create_complete(c(5,5))))
  expect_s3_class(create_complete(4), "igraph")
  expect_error(create_complete(c(5,5,5)), "single integer")
  expect_false(is_directed(create_complete(6)))
  expect_false(is_directed(create_complete(c(5,5))))
})

test_that("ring creation works", {
  expect_true(is_twomode(create_ring(c(5,5))))
  expect_equal(unname(as_matrix(create_ring(3))), 
               matrix(c(0,1,1,1,0,1,1,1,0),3,3))
  expect_equal(unname(as_matrix(create_ring(c(5,5), width = 1))), 
               matrix(c(1,0,0,0,1,1,1,0,0,0,0,1,1,0,0,0,0,1,1,0,0,0,0,1,1),5,5))
  expect_equal(unname(as_matrix(create_ring(c(5,5), width = 2))), 
               matrix(c(1,0,0,1,1,1,1,0,0,1,1,1,1,0,0,0,1,1,1,0,0,0,1,1,1),5,5))
  expect_error(create_ring(c(5,5,5)), "single integer")
})

test_that("component creation works", {
  expect_true(is_twomode(create_components(c(5,5))))
  expect_equal(unname(as_matrix(create_components(4))), matrix(c(0,1,0,0,1,0,0,0,0,0,0,1,0,0,1,0),4,4))
  expect_equal(unname(as_matrix(create_components(c(2,4)))), matrix(c(1,0,1,0,0,1,0,1),2,4))
  expect_error(create_components(c(5,5,5)), "single integer")
})

test_that("star creation works", {
  expect_true(!is_twomode(create_star(5)))
  expect_true(is_twomode(create_star(c(5,5))))
  expect_equal(unname(as_matrix(create_star(c(2,2), FALSE))), matrix(c(1,1,0,0),2,2))
  expect_equal(unname(as_matrix(create_star(c(2,2), directed = TRUE))), matrix(c(1,0,1,0),2,2))
  expect_error(create_star(c(5,5,5)), "single integer")
})

test_that("core-periphery creation works", {
  expect_false(is_twomode(create_core(6)))
  expect_true(is_twomode(create_core(c(6,6))))
  expect_equal(network_nodes(create_core(c(10,4))), 14)
})

# test_that("nest creation works", {
#   expect_equal(create_nest(2,4, as = "matrix"), matrix(c(1,1,0,1,0,0,0,0),2,4))
#   expect_s3_class(create_nest(2,4, as = "igraph"), "igraph")
#   expect_s3_class(create_nest(2,4, as = "tidygraph"), "tbl_graph")
# })

test_that("create lattice works", {
  expect_s3_class(create_lattice(4), "igraph")
  expect_equal(network_nodes(create_lattice(5)), 5)
  expect_false(is_directed(create_lattice(6)))
  expect_true(is_directed(create_lattice(6, directed = TRUE)))
})
