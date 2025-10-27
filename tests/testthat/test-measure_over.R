test_that("over_waves works", {
  res <- over_waves(manynet::fict_potter, manynet::net_components)
  expect_equal(unname(unlist(c(res))), c(58,48,52,57,43,54))
})

test_that("over_membership works", {
  res <- over_membership(fict_potter, manynet::net_assortativity, 
                         membership = node_in_regular(fict_potter))
  expect_equal(unname(unlist(c(res))), c(0.490201713,NaN))
})
