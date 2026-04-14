test_that("run_tute() runs without error when missing argument", {
  skip_if_not_installed("learnr")
  expect_error(run_tute(), NA)
})

test_that("extract_tute() runs without error when missing argument", {
  skip_if_not_installed("learnr")
  expect_error(extract_tute(), NA)
})