test_that("random creation works", {
  expect_false(isTRUE(all.equal(sample_affiliation(c(2,4),.3), sample_affiliation(c(2,4),.3))))
})
