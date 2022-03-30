# Census function family tests
set.seed(123)
task_eg <- to_named(to_uniplex(ison_algebra, "task_tie"))
test <- node_tie_census(task_eg)

test_that("nodetie census works", {
  expect_equal(test[1:4], rep(0, 4))
  expect_equal(class(test)[[1]], "matrix")
  expect_equal(class(node_tie_census(ison_karateka)), c("matrix","array"))
})

test <- node_triad_census(task_eg)
test_that("node triad census works", {
  expect_equal(test[1:4], c(36, 45, 55, 102))
  expect_equal(class(test)[[1]], "matrix")
  expect_equal(colnames(test)[1:3], c("003", "012", "102"))
  expect_equal(class(node_triad_census(ison_karateka)), c("matrix","array"))
})

test <- group_tie_census(task_eg,
                         cutree(cluster_structural_equivalence(task_eg), 4))
test_that("group tie census works", {
  expect_equal(test[1:4], c(0.08, 0.15, 0.00, 5.10))
  expect_equal(class(test)[[1]], "matrix")
  expect_equal(rownames(test)[1:4],
               c("Block 1",
                 "Block 2",
                 "Block 3",
                 "Block 4"))
  expect_equal(substr(colnames(test)[1:4], 1, 4),
               rep("from", 4))
})

test <- group_triad_census(task_eg,
                         cutree(cluster_structural_equivalence(task_eg), 4))
test_that("group triad census works", {
  expect_equal(test[1:4], c(82, 55, 80, 0))
  expect_equal(class(test)[[1]], "matrix")
  expect_equal(rownames(test)[1:4],
               c("Block 1",
                 "Block 2",
                 "Block 3",
                 "Block 4"))
  expect_equal(colnames(test)[1:3], c("003", "012", "102"))
})

test <- graph_dyad_census(ison_adolescents)
test_that("graph dyad census works", {
  expect_equal(test[[1]], 10)
  expect_equal(test[[2]], 18)
  expect_equal(names(test), c("Mutual", "Null"))
  expect_equal(class(test), "integer")
  # Error
  expect_error(graph_dyad_census(ison_southern_women))
})

test <- graph_triad_census(ison_adolescents)
test_that("graph triad census works", {
  expect_equal(test[[1]], 13)
  expect_equal(test[[3]], 29)
  expect_equal(names(test), c("003", "012", "102", "201", "210", "300"))
  expect_equal(class(test), "numeric")
  # Error
  expect_error(graph_triad_census(ison_southern_women))
})

test_that("node quad census works", {
  quad_cen <- node_quad_census(ison_southern_women)
  expect_equal(class(quad_cen), c("matrix", "array"))
  expect_equal(quad_cen[1,1], 1402)
})

test_that("graph mixed census works", {
  marvel_friends <- to_unsigned(ison_marvel_relationships, "positive")
  mixed_cen <- graph_mixed_census(marvel_friends, ison_marvel_teams)  
  expect_equal(class(mixed_cen), "numeric")
  expect_equal(unname(mixed_cen[1]), 1137)
  expect_equal(names(mixed_cen[1]), "22")
  # Errors
  expect_error(graph_mixed_census(ison_southern_women, ison_marvel_teams))
  expect_error(graph_mixed_census(ison_marvel_teams, ison_southern_women))
  expect_error(graph_mixed_census(ison_karateka, ison_marvel_teams))
})
