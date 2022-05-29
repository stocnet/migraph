test_that("graph components works", {
  expect_equal(graph_components(ison_marvel_teams), 
               igraph::components(as_igraph(ison_marvel_teams))$no)
  expect_true(class(graph_components(ison_marvel_teams)) == "integer")
})

test_that("graph cohesion works", {
  expect_length(graph_cohesion(ison_marvel_teams), 1)
  expect_equal(class(graph_cohesion(ison_adolescents)), "numeric")
})

test_that("graph adhesion works", {
  expect_length(graph_adhesion(ison_algebra), 1)
  expect_equal(class(graph_adhesion(ison_southern_women)), "numeric")
})

test_that("graph diameter works", {
  expect_length(graph_diameter(ison_marvel_relationships), 1)
  expect_equal(class(graph_diameter(ison_adolescents)), "numeric")
})

test_that("graph length works", {
  expect_length(graph_length(ison_marvel_relationships), 1)
})
