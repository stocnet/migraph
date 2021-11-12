#*************** Test the diversity family of functions ***********************#

test_that("Blau index function works", {
  expect_equal(round(graph_blau_index(ison_marvel_relationships, "Gender"), 2), 0.31)
  expect_equal(lapply(graph_blau_index(ison_marvel_relationships, "Gender", "Rich"), round, 2),
               list(`Cluster 0` = 0.34, `Cluster 1` = 0.17) )
})

test_that("EI index function works", {
expect_equal(round(graph_ei_index(ison_marvel_relationships, "Gender"), 2), -0.39)
})
