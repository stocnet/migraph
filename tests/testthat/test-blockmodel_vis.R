# Tests for blockmodel visualization
set.seed(123)

testplot <- plot(blockmodel_concor(mpn_elite_mex))
test_that("blockmodelviz works", {
  expect_equal(class(testplot)[1], "gg")
  expect_equal(length(testplot$layers), 3)
})
