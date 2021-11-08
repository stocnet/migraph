# Tests for blockmodel visualization
usa_concor <- blockmodel_concor(mpn_elite_usa_advice)
test <- plot(usa_concor)

test_that("blockmodelviz works", {
  expect_equal(test$data$Var1[1], "Albright")
  expect_equal(test$data$Var2[1], "ACUS")
  expect_equal(test$data$value[1], 0)
  expect_equal(class(test)[1], "gg")
  expect_equal(length(test$layers), 3)
})

res <- cluster_regular_equivalence(mpn_elite_mex)
test <- ggtree(res, 4)
test_that("blockmodelviz works", {
  expect_equal(class(test)[1], "gg")
  expect_equal(length(test$layers), 3)
})

res <- cluster_regular_equivalence(mpn_elite_mex)
test <- ggidentify_clusters(res, node_triad_census(mpn_elite_mex))
test_that("blockmodelviz works", {
  expect_equal(class(test)[1], "gg")
  expect_equal(length(test$layers), 2)
  expect_equal(test$data$clusters, c(2, 3, 4, 5 , 6, 7, 8, 9, 10, 11))
  expect_equal(round(test$data$correlations[1:2], 2), c(0.83, 0.92))
  expect_equal(test$data$correct[1:3], c("#6f7072", "#6f7072", "#E20020"))
})
