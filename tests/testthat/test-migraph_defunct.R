# Testing deprecated functions

# ggidentify() ----

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

# graph_clustering() ----
test_that("graph_clustering is deprecated", {
  expect_warning(graph_clustering(brandes))
})

# cluster_triad_census() ----
test_that("cluster_triad_census is deprecated", {
  set.seed(123)
  task_eg <- to_named(to_uniplex(ison_m182, "task_tie"))
  expect_warning(cluster_triad_census(task_eg,
                                      cutree(cluster_structural_equivalence(task_eg), 4)))})

# ggraphgrid() ----
test_that("ggraphgrid is deprecated", {
  expect_warning(ggraphgrid(brandes, algorithm = "fr"))
  expect_warning(ggraphgrid(brandes, algorithm = "kk"))
})

# test_cug() ----
test_that("test_cug is deprecated", {
  marvel_friends <- to_unsigned(ison_marvel_relationships)
  marvel_friends <- to_main_component(marvel_friends)
  marvel_friends <- dplyr::filter(marvel_friends, PowerOrigin == "Human")
  expect_error(suppressWarnings(test_cug(marvel_friends,
                      graph_ei_index,
                      nSim = 200,
                      attribute = "Attractive")))
})

#  graph_dimensions() ----
test_that("graph_dimensions is deprecated", {
  expect_warning(graph_dimensions(brandes))
})

#  netlm() ----
test_that("netlm() is deprecated", {
  expect_warning(netlm(weight ~ ego(Citations) + alter(Citations) + same(Discipline), ison_eies))
})
