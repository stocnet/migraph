graph1 <- igraph::make_directed_graph(c(1,2,1,5,2,3,2,4,3,5,4,5,5,1))
graph2 <- igraph::make_undirected_graph(c(1,1,1,2,2,4,3,4,3,4))

test_that("edge_reciprocal works", {
  expect_length(edge_reciprocal(graph1), 7)
  expect_true(edge_reciprocal(graph1)[2], edge_reciprocal(graph1)[7])
})

test_that("edge_multiple works", {
  expect_true(edge_multiple(graph2)[5])
})

test_that("edge_loop works", {
  expect_true(edge_loop(graph2)[1])
  expect_false(edge_loop(graph2)[2], edge_loop(graph2)[3], edge_loop(graph2)[4])
})
