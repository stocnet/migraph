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
  expect_equal(test[1:4], c(36, 45, 55, 102))
  expect_s3_class(test, "node_motif")
  expect_equal(colnames(test)[1:3], c("003", "012", "102"))
})

test <- group_tie_census(task_eg, node_structural_equivalence(task_eg), 4)
test_that("group tie census works", {
  expect_equal(class(test), c("matrix", "array"))
  expect_equal(rownames(test)[1:4],
               c("Block 1",
                 "Block 2",
                 "Block 3",
                 "Block 4"))
  expect_equal(substr(colnames(test)[1:4], 1, 4),
               rep("from", 4))
})

test <- group_triad_census(task_eg, node_structural_equivalence(task_eg), 4)
test_that("group triad census works", {
  expect_equal(class(test), c("matrix", "array"))
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
  expect_s3_class(test, "graph_motif")
  # Error
  expect_error(graph_dyad_census(ison_southern_women))
})

test <- graph_triad_census(ison_adolescents)
test_that("graph triad census works", {
  expect_equal(test[[1]], 13)
  expect_equal(test[[3]], 29)
  expect_equal(names(test), c("003", "012", "102", "201", "210", "300"))
  expect_s3_class(test, "graph_motif")
  # Error
  expect_error(graph_triad_census(ison_southern_women))
})

test <- node_quad_census(ison_southern_women)
test_that("node quad census works", {
  expect_s3_class(test, "node_motif")
  expect_equal(test[1,1], 1402)
})

test_that("graph mixed census works", {
  marvel_friends <- to_unsigned(ison_marvel_relationships, "positive")
  test <- graph_mixed_census(marvel_friends, ison_marvel_teams)  
  expect_s3_class(test, "graph_motif")
  expect_equal(unname(test[1]), 1137)
  expect_equal(names(test[1]), "22")
  # Errors
  expect_error(graph_mixed_census(ison_southern_women, ison_marvel_teams))
  expect_error(graph_mixed_census(ison_marvel_teams, ison_southern_women))
  expect_error(graph_mixed_census(ison_karateka, ison_marvel_teams))
})

test <- node_path_census(ison_southern_women)
test_that("node path census works", {
  expect_equal(graph_nodes(ison_adolescents),
               nrow(node_path_census(ison_adolescents)))
  expect_s3_class(test, "node_motif")
  expect_true(nrow(node_path_census(ison_southern_women)) == ncol(node_path_census(ison_southern_women)))
})
