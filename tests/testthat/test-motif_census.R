# # Census function family tests
set.seed(123)
task_eg <- manynet::to_named(manynet::to_uniplex(manynet::ison_algebra, "tasks"))

test <- node_tie_census(task_eg)
test_that("node tie census works", {
  expect_equal(test[1:4], rep(0, 4))
  expect_s3_class(test, "node_motif")
})

test <- node_triad_census(task_eg)
test_that("node triad census works", {
  expect_equal(top3(test[,16]), c(7,8,6))
  expect_s3_class(test, "node_motif")
  expect_equal(colnames(test)[1:3], c("003", "012", "102"))
})

test <- network_dyad_census(manynet::ison_adolescents)
test_that("network_dyad census works", {
  expect_equal(test[[1]], 10)
  expect_equal(test[[2]], 18)
  expect_equal(names(test), c("Mutual", "Null"))
  expect_s3_class(test, "network_motif")
  # Error
  expect_error(network_dyad_census(manynet::ison_southern_women))
})

test <- network_triad_census(manynet::ison_adolescents)
test_that("network_triad census works", {
  expect_equal(test[[1]], 13)
  expect_equal(test[[3]], 29)
  expect_equal(names(test), c("003", "012", "102", "201", "210", "300"))
  expect_s3_class(test, "network_motif")
  # Error
  expect_error(network_triad_census(manynet::ison_southern_women))
})

test <- node_quad_census(manynet::ison_southern_women)
test_that("node quad census works", {
  expect_s3_class(test, "node_motif")
  expect_equal(test[1,1], 1402)
})

test_that("network_mixed census works", {
  marvel_friends <- to_unsigned(manynet::ison_marvel_relationships, "positive")
  test <- network_mixed_census(marvel_friends, manynet::ison_marvel_teams)
  expect_s3_class(test, "network_motif")
  expect_equal(unname(test[1]), 1137)
  expect_equal(names(test[1]), "22")
  # Errors
  expect_error(network_mixed_census(manynet::ison_southern_women,
                                    manynet::ison_marvel_teams))
  expect_error(network_mixed_census(manynet::ison_marvel_teams,
                                    manynet::ison_southern_women))
  expect_error(network_mixed_census(manynet::ison_karateka,
                                    manynet::ison_marvel_teams))
})

test <- node_path_census(manynet::ison_southern_women)
test_that("node path census works", {
  expect_equal(network_nodes(manynet::ison_adolescents),
               nrow(node_path_census(manynet::ison_adolescents)))
  expect_s3_class(test, "node_motif")
  expect_true(nrow(node_path_census(manynet::ison_southern_women)) ==
                ncol(node_path_census(manynet::ison_southern_women)))
})

test <- node_brokering_activity(manynet::ison_networkers, "Discipline")
test_that("node activity works", {
  expect_s3_class(test, "node_measure")
  expect_equal(manynet::network_nodes(manynet::ison_networkers), length(test))
  expect_equal(top3(test), c(333,207,3))
})

test <- node_brokering_exclusivity(manynet::ison_networkers, "Discipline")
test_that("node exclusivity works", {
  expect_s3_class(test, "node_measure")
  expect_equal(manynet::network_nodes(manynet::ison_networkers), length(test))
  expect_equal(top3(test), c(1,0,0))
})

test <- node_brokering(manynet::ison_networkers, "Discipline")
test_that("node brokering works", {
  expect_s3_class(test, "node_member")
  expect_equal(manynet::network_nodes(manynet::ison_networkers), length(test))
  expect_equal(top3(test), c("Powerhouse","Connectors","Sideliners"))
})
