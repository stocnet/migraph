#*************** Test the diversity family of functions ***********************#

test_that("Blau index function works", {
  expect_equal(as.numeric(graph_blau_index(ison_marvel_relationships, "Gender")), 0.306, tolerance = 0.001)
  expect_equal(as.numeric(graph_blau_index(ison_marvel_relationships, "Gender", "Rich")),
               c(0.337,0.165), tolerance = 0.001)
})

test_that("EI index function works", {
  expect_equal(as.numeric(graph_ei_index(mpn_elite_mex, "military")), -0.3675, tolerance = 0.001)
})

test_that("graph_assortativity function works", {
  expect_length(graph_assortativity(mpn_elite_mex), 1)
  expect_s3_class(graph_assortativity(mpn_elite_mex), "graph_measure")
})
