# Tests for reexport_classes.R

test_that("expect_nodes and expect_edges works", {
  expect_error(expect_nodes())
  expect_error(expect_edges())
})
