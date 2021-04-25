test_that("random creation works", {
  expect_false(isTRUE(all.equal(generate_random(4,.3), generate_random(4,.3))))
  expect_false(isTRUE(all.equal(generate_random(c(2,4),.3), generate_random(c(2,4),.3))))
  expect_error(generate_random(c(1,2,3)), "must be of length")
})
