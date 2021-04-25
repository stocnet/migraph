test_that("random creation works", {
  expect_false(isTRUE(all.equal(generate_random(c(2,4),.3), generate_random(c(2,4),.3))))
})
