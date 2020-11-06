test_that("lattice creation works", {
  expect_equal(create_chain(2,4, as = "matrix"), matrix(1,2,4))
})

test_that("silo creation works", {
  expect_equal(create_silos(2,4, as = "matrix"), matrix(c(1,0,1,0,0,1,0,1),2,4))
})

test_that("match creation works", {
  expect_equal(create_match(2,4, as = "matrix"), matrix(c(1,0,0,1,1,0,0,1),2,4))
})

test_that("nest creation works", {
  expect_equal(create_nest(2,4, as = "matrix"), matrix(c(1,1,0,1,0,0,0,0),2,4))
})

test_that("random creation works", {
  expect_false(isTRUE(all.equal(play_twomode(2,4,.3), play_twomode(2,4,.3))))
})
