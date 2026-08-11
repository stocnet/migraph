test_that("over_waves works", {
  res <- over_waves(manynet::fict_potter, netrics::net_by_components)
  # expect_equal(unname(unlist(c(res))), c(48,52,57,43,54,64))
})

test_that("over_membership works", {
  # A fixed two-block network with an explicit membership, so that the
  # expectations do not move when upstream clustering methods change.
  # Block A is a triad plus a pendant, block B a path, joined by one cross tie.
  el <- rbind(c(1,2), c(1,3), c(1,4), c(2,3), c(5,6), c(6,7), c(7,8), c(4,5))
  mat <- matrix(0, 8, 8)
  mat[el] <- 1
  mat[el[, 2:1]] <- 1
  memb <- rep(c("A","B"), each = 4)
  # Densities check the blocks are split as expected, the cross tie excluded:
  # 4 of 6 possible ties within A, 3 of 6 within B.
  expect_equal(unname(unlist(c(over_membership(mat, netrics::net_by_density,
                                               membership = memb)))),
               c(4/6, 3/6))
  expect_equal(unname(unlist(c(over_membership(mat, netrics::net_by_assortativity,
                                               membership = memb)))),
               c(-5/7, -1/2))
})
