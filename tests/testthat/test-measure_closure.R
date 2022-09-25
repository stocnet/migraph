test_that("network density works", {
  expect_s3_class(network_density(ison_southern_women), "network_measure")
  expect_equal(as.numeric(network_density(create_empty(10))), 0)
  expect_equal(as.numeric(network_density(create_empty(c(10,6)))), 0)
  expect_equal(as.numeric(network_density(create_complete(10))), 1)
  expect_equal(as.numeric(network_density(create_complete(c(10,6)))), 1)
  expect_output(print(network_density(create_complete(10))))
})

test_that("network reciprocity works", {
  expect_s3_class(network_reciprocity(ison_networkers), "network_measure")
  expect_output(print(network_reciprocity(ison_networkers)))
  expect_length(network_reciprocity(ison_networkers), 1)
  expect_equal(as.numeric(network_reciprocity(ison_networkers)),
               igraph::reciprocity(as_igraph(ison_networkers)))
})

test_that("one-mode object clustering is reported correctly",{
  expect_equal(as.numeric(network_transitivity(ison_algebra)),
               0.69787, tolerance = 0.001)
  expect_s3_class(network_transitivity(ison_algebra), "network_measure")
  expect_output(print(network_transitivity(ison_algebra)))
})

test_that("two-mode object clustering is reported correctly",{
  expect_equal(as.numeric(network_equivalency(ison_southern_women)),
               0.4872, tolerance = 0.001)
  expect_s3_class(network_equivalency(ison_southern_women), "network_measure")
  expect_output(print(network_equivalency(ison_southern_women)))
})

test_that("three-mode clustering calculated correctly",{
  mat1 <- create_ring(5,10)
  mat2 <- create_ring(5,8)
  expect_equal(as.numeric(network_congruency(mat1, mat2)),
               0.7143, tolerance = 0.001)
  expect_s3_class(network_congruency(mat1, mat2), "network_measure")
  expect_output(print(network_congruency(mat1, mat2)))
})

