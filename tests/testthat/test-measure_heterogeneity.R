#*************** Test the heterogeneity family of functions ******************#

test_that("diversity function works", {
  expect_equal(as.numeric(network_diversity(ison_marvel_relationships, "Gender")), 0.306, tolerance = 0.001)
  expect_equal(as.numeric(network_diversity(ison_marvel_relationships, "Gender", "Rich")),
               c(0.337,0.165), tolerance = 0.001)
})

test_that("heterophily function works", {
  expect_equal(as.numeric(network_heterophily(mpn_elite_mex, "military")), -0.3675, tolerance = 0.001)
  expect_length(node_heterophily(mpn_elite_mex, "military"),
                network_nodes(mpn_elite_mex))
  expect_s3_class(node_heterophily(mpn_elite_mex, "military"), "node_measure")
})

test_that("assortativity function works", {
  expect_length(network_assortativity(mpn_elite_mex), 1)
  expect_s3_class(network_assortativity(mpn_elite_mex), "network_measure")
})

test_that("richeness function works", {
  expect_length(network_richness(mpn_bristol), 1)
  expect_equal(as.numeric(network_richness(mpn_bristol)), 3)
  expect_s3_class(network_richness(mpn_bristol), "network_measure")
  expect_length(node_richness(mpn_bristol, "type"), 264)
  expect_s3_class(node_richness(mpn_bristol, "type"), "node_measure")
})
