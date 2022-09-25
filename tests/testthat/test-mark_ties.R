graph1 <- igraph::make_directed_graph(c(1,2,1,5,2,3,2,4,3,5,4,5,5,1))
graph2 <- igraph::make_undirected_graph(c(1,1,1,2,2,4,3,4,3,4))

test_that("tie_is_reciprocated works", {
  expect_length(tie_is_reciprocated(graph1), 7)
  expect_true(tie_is_reciprocated(graph1)[2], tie_is_reciprocated(graph1)[7])
})

test_that("tie_is_multiple works", {
  expect_true(tie_is_multiple(graph2)[5])
})

test_that("tie_is_loop works", {
  expect_true(tie_is_loop(graph2)[1])
  expect_false(tie_is_loop(graph2)[2], tie_is_loop(graph2)[3], tie_is_loop(graph2)[4])
})

test_that("tie_is_bridge works", {
  expect_equal(length(tie_is_bridge(graph1)), network_ties(graph1))
})

test_that("tie_is_max works", {
  expect_equal(length(tie_is_max(tie_betweenness(ison_brandes))), network_ties(ison_brandes))
  expect_equal(sum(tie_is_max(tie_betweenness(ison_brandes)) == TRUE), 1)
  expect_s3_class(tie_is_max(tie_betweenness(ison_brandes)), "logical")
})

test_that("tie_is_min works", {
  expect_equal(length(tie_is_min(tie_betweenness(ison_brandes))), network_ties(ison_brandes))
  expect_equal(sum(tie_is_min(tie_betweenness(ison_brandes)) == TRUE), 1)
  expect_s3_class(tie_is_min(tie_betweenness(ison_brandes)), "logical")
})
