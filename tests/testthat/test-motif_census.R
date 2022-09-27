# Census function family tests
set.seed(123)
task_eg <- to_named(to_uniplex(ison_algebra, "task_tie"))

test <- node_tie_census(task_eg)
test_that("node tie census works", {
  expect_equal(test[1:4], rep(0, 4))
  expect_s3_class(test, "node_motif")
})

test <- node_triad_census(task_eg)
test_that("node triad census works", {
  expect_equal(test[1:4], c(6, 15, 20, 102))
  expect_s3_class(test, "node_motif")
  expect_equal(colnames(test)[1:3], c("003", "012", "102"))
})

test <- network_dyad_census(ison_adolescents)
test_that("network_dyad census works", {
  expect_equal(test[[1]], 10)
  expect_equal(test[[2]], 18)
  expect_equal(names(test), c("Mutual", "Null"))
  expect_s3_class(test, "network_motif")
  # Error
  expect_error(network_dyad_census(ison_southern_women))
})

test <- network_triad_census(ison_adolescents)
test_that("network_triad census works", {
  expect_equal(test[[1]], 13)
  expect_equal(test[[3]], 29)
  expect_equal(names(test), c("003", "012", "102", "201", "210", "300"))
  expect_s3_class(test, "network_motif")
  # Error
  expect_error(network_triad_census(ison_southern_women))
})

test <- node_quad_census(ison_southern_women)
test_that("node quad census works", {
  expect_s3_class(test, "node_motif")
  expect_equal(test[1,1], 1402)
})

test_that("network_mixed census works", {
  marvel_friends <- to_unsigned(ison_marvel_relationships, "positive")
  test <- network_mixed_census(marvel_friends, ison_marvel_teams)  
  expect_s3_class(test, "network_motif")
  expect_equal(unname(test[1]), 1137)
  expect_equal(names(test[1]), "22")
  # Errors
  expect_error(network_mixed_census(ison_southern_women, ison_marvel_teams))
  expect_error(network_mixed_census(ison_marvel_teams, ison_southern_women))
  expect_error(network_mixed_census(ison_karateka, ison_marvel_teams))
})

test <- node_path_census(ison_southern_women)
test_that("node path census works", {
  expect_equal(network_nodes(ison_adolescents),
               nrow(node_path_census(ison_adolescents)))
  expect_s3_class(test, "node_motif")
  expect_true(nrow(node_path_census(ison_southern_women)) == ncol(node_path_census(ison_southern_women)))
})
