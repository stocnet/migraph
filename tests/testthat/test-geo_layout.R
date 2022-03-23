# test_that("network_map works", {
#   # Load internal snapshot of manyenvir membership data
#   testnet <- migraph:::membership
#   testlight <- network_map(testnet, "2010-01-01")
#   testdark <- network_map(testnet, "2010-01-01", theme = "dark")
#   testearth <- network_map(testnet, "2010-01-01", theme = "earth")
#   expect_equal(names(testlight$data), c("ID", "x", "y", "name",
#                                         ".ggraph.orig_index", "geometry",
#                                         ".ggraph.index", "circular"))
#   expect_equal(names(testdark$data), c("ID", "x", "y", "name",
#                                         ".ggraph.orig_index", "geometry",
#                                         ".ggraph.index", "circular"))
#   expect_equal(names(testearth$data), c("ID", "x", "y", "name",
#                                         ".ggraph.orig_index", "geometry",
#                                         ".ggraph.index", "circular"))
# })
