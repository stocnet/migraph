# Create test plots
# testDegree <- ggidentify(brandes, node_degree)
# testBetweeness <- ggidentify(brandes, node_betweenness)
# testCloseness <- ggidentify(brandes, node_closeness)
# testEigen <- ggidentify(brandes, node_eigenvector)

# Step 1: Test deprecation warning

test_that("ggidentify is deprecated", {
  expect_warning(ggidentify(brandes, node_degree))
  expect_warning(ggidentify(brandes, node_betweenness))
  expect_warning(ggidentify(brandes, node_closeness))
  expect_warning(ggidentify(brandes, node_eigenvector))
})

# Step 2: Run old tests and silence deprecation warnings

test_that("Plot identify function works", {
  expect_true(is.list(suppressWarnings(ggidentify(brandes, node_degree))))
  expect_length(suppressWarnings(ggidentify(brandes, node_degree)), 9)
  expect_named(suppressWarnings(ggidentify(brandes, node_degree))[1], "data")
  # Color setting is done via quosure so we just test its presence.
  expect_true(is.call(suppressWarnings(ggidentify(brandes, node_degree))[["layers"]][[2]][["mapping"]][["colour"]]))
})

test_that("Plot identify function works", {
  expect_true(is.list(suppressWarnings(ggidentify(brandes, node_betweenness))))
  expect_length(suppressWarnings(ggidentify(brandes, node_betweenness)), 9)
  expect_named(suppressWarnings(ggidentify(brandes, node_betweenness))[1], "data")
  expect_true(is.call(suppressWarnings(ggidentify(brandes, node_betweenness))[["layers"]][[2]][["mapping"]][["colour"]]))
})

test_that("Plot identify function works", {
  expect_true(is.list(suppressWarnings(ggidentify(brandes, node_closeness))))
  expect_length(suppressWarnings(ggidentify(brandes, node_closeness)), 9)
  expect_named(suppressWarnings(ggidentify(brandes, node_closeness))[1], "data")
  expect_true(is.call(suppressWarnings(ggidentify(brandes, node_closeness))[["layers"]][[2]][["mapping"]][["colour"]]))
})

test_that("Plot identify function works", {
  expect_true(is.list(suppressWarnings(ggidentify(brandes, node_eigenvector))))
  expect_length(suppressWarnings(ggidentify(brandes, node_eigenvector)), 9)
  expect_named(suppressWarnings(ggidentify(brandes, node_eigenvector))[1], "data")
  expect_true(is.call(suppressWarnings(ggidentify(brandes, node_eigenvector))
[["layers"]][[2]][["mapping"]][["colour"]]))
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