test_that("constraint scores are reported correctly for two-mode notworks",{
  expect_equal(round(unname(node_constraint(ison_southern_women)[1:3]),2), c(0.28, 0.31, 0.29))
  expect_named(node_constraint(ison_southern_women)[1:3], c("EVELYN", "LAURA", "THERESA"))
})

om <- igraph::graph(edges = c(1,2, 2,3), n = 4, directed = FALSE) 

test_that("constraint scores are reported correctly for one-mode notworks",{
  expect_equal(round(unname(node_constraint(mpn_elite_mex)[1:3]),2), c(0.45, 0.35, 0.28))
})


testDegree     <- plot(node_degree(ison_brandes))
testBetweeness <- plot(node_betweenness(ison_brandes))
testCloseness  <- plot(node_closeness(ison_brandes))
testEigen      <- plot(node_eigenvector(ison_brandes))

test_that("Plot identify function works", {
  expect_true(is.list(testEigen))
  expect_length(testDegree, 9)
  expect_named(testDegree[1], "data")
  expect_equal(testEigen[["labels"]][["y"]], "Frequency")
  expect_equal(testBetweeness[["labels"]][["x"]], "Score")
})
