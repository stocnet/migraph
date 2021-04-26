test_that("concor works on one-mode networks", {
  expect_equal(blockmodel_concor(mpn_elite_mex)$membership, c(1,1,2,2,2,1,2,2,2,2,1))
  expect_equal(blockmodel_concor(mpn_elite_mex, p = 2)$membership, c(1,1,3,3,4,2,3,4,3,4,2))
  expect_equal(blockmodel_concor(mpn_elite_mex, p = 3)$membership, c(1,2,5,6,7,3,5,8,5,7,4))
})

test_that("concor works on two-mode networks", {
  expect_equal(blockmodel_concor(mpn_elite_usa_advice)$membership$nodes1, c(1,2,2,1,1,2,1,1,2,1,2,2,2,2))
  expect_equal(blockmodel_concor(mpn_elite_usa_advice, p = 2)$membership$nodes1, c(1,3,3,2,2,4,1,1,3,1,3,4,3,3))
  expect_error(blockmodel_concor(mpn_elite_usa_advice, p = 3)$membership$nodes1, "differing number of rows")
  expect_equal(blockmodel_concor(mpn_elite_usa_advice)$membership$nodes2, c(1,2,2,1,2,1,2,1,2,1,1,1,2,2,2,1,2,2,1,1))
  expect_equal(blockmodel_concor(mpn_elite_usa_advice, p = 2)$membership$nodes2, c(1,3,4,2,4,2,4,1,4,1,1,1,4,3,4,1,3,3,2,2))
  expect_error(blockmodel_concor(mpn_elite_usa_advice, p = 3)$membership$nodes2, "differing number of rows")
})

test_that("blockmodel print works correctly", {
  expect_output(print(blockmodel_concor(mpn_elite_usa_advice, p = 2)), "Network Blockmodel:")
  expect_output(print(blockmodel_concor(mpn_elite_usa_advice, p = 2)), "First nodeset:")
})