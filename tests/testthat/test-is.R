test_that("is tests work", {
  expect_true(is_twomode(southern_women))
  expect_false(is_directed(southern_women))
  expect_false(is_weighted(southern_women))
  expect_true(is_labelled(southern_women))
})
