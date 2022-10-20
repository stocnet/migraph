#*************** Test the diversity family of functions ***********************#

test_that("Blau index function works", {
  expect_equal(as.numeric(network_diversity(ison_marvel_relationships, "Gender")), 0.306, tolerance = 0.001)
  expect_equal(as.numeric(network_diversity(ison_marvel_relationships, "Gender", "Rich")),
               c(0.337,0.165), tolerance = 0.001)
})

test_that("EI index function works", {
  expect_equal(as.numeric(network_homophily(mpn_elite_mex, "military")), -0.3675, tolerance = 0.001)
})

test_that("node_homophily function works", {
  expect_length(node_homophily(mpn_elite_mex, "military"),
                network_nodes(mpn_elite_mex))
  expect_s3_class(node_homophily(mpn_elite_mex, "military"), "node_measure")
})

test_that("network_assortativity function works", {
  expect_length(network_assortativity(mpn_elite_mex), 1)
  expect_s3_class(network_assortativity(mpn_elite_mex), "network_measure")
})
