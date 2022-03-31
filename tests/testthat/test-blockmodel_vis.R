# Tests for blockmodel visualization
set.seed(123)

res <- cluster_regular_equivalence(ison_adolescents)
test <- ggtree(res, 4)
test_that("blockmodelviz works", {
  expect_equal(class(test)[1], "gg")
  expect_equal(length(test$layers), 3)
})

test <- ggidentify_clusters(res, node_triad_census(ison_adolescents))
test_that("blockmodelviz works", {
  expect_equal(class(test)[1], "gg")
  expect_equal(length(test$layers), 2)
  expect_equal(test$data$clusters[1:7], c(2, 3, 4, 5 , 6, 7, 8))
  expect_equal(round(test$data$correlations[1:2], 2), c(0.91, 0.98), tolerance = 0.05)
})
