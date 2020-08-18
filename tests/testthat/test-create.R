test_that("lattice creation works", {
  expect_equal(create_lattice(2,4), matrix(1,2,4))
})

test_that("poles creation works", {
  expect_equal(create_poles(2,4), matrix(c(1,0,1,0,0,1,0,1),2,4))
})

test_that("match creation works", {
  expect_equal(create_match(2,4), matrix(c(1,0,0,1,1,0,0,1),2,4))
})

test_that("nest creation works", {
  expect_equal(create_nest(2,4), matrix(c(1,1,0,1,0,0,0,0),2,4))
})

test_that("random creation works", {
  expect_false(isTRUE(all.equal(create_random(2,4,.3), create_random(2,4,.3))))
})
