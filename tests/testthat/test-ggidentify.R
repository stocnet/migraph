# Create test plots
testDegree <- ggidentify(brandes, node_degree)
testBetweeness <- ggidentify(brandes, node_betweenness)
testCloseness <- ggidentify(brandes, node_closeness)
testEigen <- ggidentify(brandes, node_eigenvector)

test_that("Plot identify function works", {
  expect_true(is.list(testDegree))
  expect_length(testDegree, 9)
  expect_named(testDegree[1], "data")
  # Color setting is done via quosure so we just test its presence.
  expect_true(is.call(testDegree[["layers"]][[2]][["mapping"]][["colour"]]))
})

test_that("Plot identify function works", {
  expect_true(is.list(testBetweeness))
  expect_length(testDegree, 9)
  expect_named(testDegree[1], "data")
  expect_true(is.call(testBetweeness[["layers"]][[2]][["mapping"]][["colour"]]))
})

test_that("Plot identify function works", {
  expect_true(is.list(testCloseness))
  expect_length(testDegree, 9)
  expect_named(testDegree[1], "data")
  expect_true(is.call(testCloseness[["layers"]][[2]][["mapping"]][["colour"]]))
})

test_that("Plot identify function works", {
  expect_true(is.list(testEigen))
  expect_length(testDegree, 9)
  expect_named(testDegree[1], "data")
  expect_true(is.call(testEigen[["layers"]][[2]][["mapping"]][["colour"]]))
})

testDegree     <- ggdistrib(brandes, node_degree)
testBetweeness <- ggdistrib(brandes, node_betweenness)
testCloseness  <- ggdistrib(brandes, node_closeness)
testEigen      <- ggdistrib(brandes, node_eigenvector)

test_that("Plot identify function works", {
  expect_true(is.list(testEigen))
  expect_length(testDegree, 9)
  expect_named(testDegree[1], "data")
  expect_equal(testEigen[["labels"]][["y"]], "Frequency")
  expect_equal(testBetweeness[["labels"]][["x"]], "Score")
})