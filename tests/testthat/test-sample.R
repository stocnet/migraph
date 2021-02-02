test_that("random creation works", {
  expect_false(isTRUE(all.equal(sample_affiliation(2,4,.3), sample_affiliation(2,4,.3))))
})
