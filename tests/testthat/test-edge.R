graph1 <- igraph::make_directed_graph(c(1,2,1,5,2,3,2,4,3,5,4,5,5,1))
graph2 <- igraph::make_undirected_graph(c(1,1,1,2,2,4,3,4,3,4))

test_that("edge_mutual works", {
  expect_length(edge_mutual(graph1), 7)
  expect_true(edge_mutual(graph1)[2], edge_mutual(graph1)[7])
})

test_that("edge_multiple works", {
  expect_true(edge_multiple(graph2)[5])
})

test_that("edge_loop works", {
  expect_true(edge_loop(graph2)[1])
  expect_false(edge_loop(graph2)[2], edge_loop(graph2)[3], edge_loop(graph2)[4])
})

test_that("edge_betweenness works", {
  expect_s3_class(edge_betweenness(ison_adolescents), 
                  "measure")
  expect_length(edge_betweenness(ison_adolescents), 
                graph_edges(ison_adolescents))
  expect_equal(unname(edge_betweenness(ison_adolescents)[1:3]), 
               c(7,3,5), tolerance = 0.001)
})

test_that("edge_closeness works", {
  expect_s3_class(edge_closeness(ison_adolescents), 
               "measure")
  expect_length(edge_closeness(ison_adolescents), 
               graph_edges(ison_adolescents))
  expect_equal(unname(edge_closeness(ison_adolescents)[1:3]), 
               c(0.0714,0.0667,0.0769), tolerance = 0.001)
})
