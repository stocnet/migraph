# Clustering tests
test_that("structural equivalence clustering works", {
  expect_equal(cluster_structural_equivalence(mpn_elite_mex)$method, "complete")
  expect_equal(cluster_structural_equivalence(mpn_elite_mex)$merge[1,], c(-3, -9))
  expect_equal(round(cluster_structural_equivalence(mpn_elite_mex)$height[1:2], 2), c(0.23, 0.48))
  expect_equal(cluster_structural_equivalence(mpn_elite_mex)$labels[1], "Alvarezx")
})

test_that("regular equivalence clustering works", {
  expect_equal(cluster_regular_equivalence(mpn_elite_mex)$method, "complete")
  expect_equal(cluster_regular_equivalence(mpn_elite_mex)$merge[1,], c(-1, -3))
  expect_equal(cluster_regular_equivalence(mpn_elite_mex)$order[1:2], c(5, 6))
  expect_equal(round(cluster_regular_equivalence(mpn_elite_mex)$height[1:2], 2), c(0, 0))
  expect_equal(cluster_regular_equivalence(mpn_elite_mex)$labels[1], "Alvarezx")
})
