# Clustering tests
test_that("structural equivalence clustering works", {
  expect_equal(cluster_structural_equivalence(mpn_elite_mex)$method, "complete")
  expect_equal(cluster_structural_equivalence(mpn_elite_mex)$merge[1,], c(-23, -24))
  expect_equal(round(cluster_structural_equivalence(mpn_elite_mex)$height[1:2], 2), c(0.25, 0.34))
  expect_equal(cluster_structural_equivalence(mpn_elite_mex)$labels[1], "Trevino")
})

test_that("regular equivalence clustering works", {
  expect_equal(cluster_regular_equivalence(mpn_elite_mex)$method, "complete")
  expect_equal(cluster_regular_equivalence(mpn_elite_mex)$merge[1,], c(-10, -13))
  expect_equal(cluster_regular_equivalence(mpn_elite_mex)$order[1:2], c(3, 2))
  expect_equal(round(cluster_regular_equivalence(mpn_elite_mex)$height[1:2], 2), c(0, 0))
  expect_equal(cluster_regular_equivalence(mpn_elite_mex)$labels[1], "Trevino")
})
