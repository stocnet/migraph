set.seed(123)

test_that("small-world metrics for two mode networks are calculated and displayed correctly", {
  expect_s3_class(network_smallworld(ison_southern_women), "network_measure")
  expect_equal(as.numeric(network_smallworld(ison_southern_women)), -1.04, tolerance = 0.02)
})

test_that("network_balance works", {
  expect_s3_class(network_balance(ison_marvel_relationships), "network_measure")
  expect_equal(as.numeric(network_balance(ison_marvel_relationships)), 0.668, tolerance = 0.01)
  expect_length(network_balance(ison_marvel_relationships), 1)
  expect_error(network_balance(ison_adolescents))
})

test_that("network_modularity works for two mode networks", {
  expect_s3_class(network_modularity(ison_southern_women,
                                   node_kernighanlin(ison_southern_women)), "network_measure")
  expect_length(network_modularity(ison_southern_women,
                                 node_kernighanlin(ison_southern_women)), 1)
})

test_that("network_core works", {
  expect_s3_class(network_core(ison_adolescents), "network_measure")
  expect_equal(length(network_core(ison_adolescents)),
               length(network_core(ison_southern_women)))
})

test_that("network_factions works", {
  expect_s3_class(network_factions(ison_adolescents), "network_measure")
  expect_equal(length(network_factions(ison_adolescents)),
               length(network_factions(ison_southern_women)))
})
