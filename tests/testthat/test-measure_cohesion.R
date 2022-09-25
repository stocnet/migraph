test_that("graph components works", {
  expect_s3_class(network_components(mpn_bristol), "network_measure")
  expect_equal(as.numeric(network_components(mpn_bristol)), 3)
})

test_that("graph cohesion works", {
  expect_s3_class(network_cohesion(mpn_bristol), "network_measure")
  expect_equal(as.numeric(network_cohesion(mpn_bristol)), 0)
})

test_that("graph adhesion works", {
  expect_s3_class(network_adhesion(mpn_bristol), "network_measure")
  expect_equal(as.numeric(network_adhesion(mpn_bristol)), 0)
})

test_that("graph diameter works", {
  expect_s3_class(network_diameter(mpn_bristol), "network_measure")
  expect_equal(as.numeric(network_diameter(mpn_bristol)), 6)
})

test_that("graph length works", {
  expect_s3_class(network_length(mpn_bristol), "network_measure")
  expect_equal(as.numeric(network_length(mpn_bristol)), 2.451265, 
               tolerance = 0.000001)
})
