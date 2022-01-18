# Unweighted, unsigned, directed network
test_brandes <- autographr(migraph::brandes)

test_that("autographr works for unweighted, unsigned, directed networks", {
  # Node position
  expect_equal(round(test_brandes[["data"]][["x"]][[1]]), 3)
  expect_equal(round(test_brandes[["data"]][["y"]][[1]]), -1)
  # Edge parameters
  expect_equal(test_brandes[["layers"]][[1]][["aes_params"]][["edge_alpha"]], 0.4)
  expect_equal(test_brandes[["layers"]][[1]][["aes_params"]][["edge_linetype"]], "solid")
  expect_equal(test_brandes[["layers"]][[1]][["aes_params"]][["edge_colour"]], "black")
  expect_equal(as.character(test_brandes[["layers"]][[1]][["aes_params"]][["end_cap"]]), "circle")
  # Node parameters
  expect_equal(round(test_brandes[["layers"]][[2]][["aes_params"]][["size"]]), 5)
  expect_equal(as.character(test_brandes[["layers"]][[2]][["aes_params"]][["shape"]]), "circle")
})

# Unweighted, signed, undirected network

test_that("autographr works for unweighted, signed, undirected networks", {
  # Node position
  expect_equal(round(test_brandes[["data"]][["x"]][[1]]), 3)
  expect_equal(round(test_brandes[["data"]][["y"]][[1]]), -1)
  # Edge parameters
  expect_equal(test_brandes[["layers"]][[1]][["aes_params"]][["edge_alpha"]], 0.4)
  expect_equal(test_brandes[["layers"]][[1]][["aes_params"]][["edge_linetype"]], "solid")
  expect_equal(test_brandes[["layers"]][[1]][["aes_params"]][["edge_colour"]], "black")
  expect_equal(as.character(test_brandes[["layers"]][[1]][["aes_params"]][["end_cap"]]), "circle")
  # Node parameters
  expect_equal(round(test_brandes[["layers"]][[2]][["aes_params"]][["size"]]), 5)
  expect_equal(as.character(test_brandes[["layers"]][[2]][["aes_params"]][["shape"]]), "circle")
})

# Weighted network

# Bipartite network