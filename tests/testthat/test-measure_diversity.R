#*************** Test the diversity family of functions ***********************#

test_that("Blau index function works", {
  expect_equal(round(graph_blau_index(ison_marvel_relationships, "Gender"), 2), 0.31)
  expect_equal(lapply(graph_blau_index(ison_marvel_relationships, "Gender", "Rich"), round, 2),
               list(`Cluster 0` = 0.34, `Cluster 1` = 0.17) )
})

test_that("EI index function works", {
  expect_equal(round(graph_ei_index(mpn_elite_mex, "military"), 2), -0.37)
})

test_that("graph_assortativity function works", {
  expect_length(graph_assortativity(mpn_elite_mex), 1)
  expect_equal(class(graph_assortativity(mpn_elite_mex)), "numeric")
})
