# Unweighted, unsigned, directed network
test_brandes <- autographr(migraph::ison_brandes)

test_that("autographr works for unweighted, unsigned, directed networks", {
  # Node position
  expect_equal(round(test_brandes[["data"]][["x"]][[1]]), 3)
  expect_equal(round(test_brandes[["data"]][["y"]][[1]]), -1)
  # Edge parameters
  expect_equal(test_brandes[["layers"]][[1]][["aes_params"]][["edge_alpha"]], 0.4)
  expect_equal(test_brandes[["layers"]][[1]][["aes_params"]][["edge_linetype"]], "solid")
  expect_equal(test_brandes[["layers"]][[1]][["aes_params"]][["edge_colour"]], "black")
  # expect_equal(as.character(test_brandes[["layers"]][[1]][["aes_params"]][["end_cap"]]), "circle")
  # Node parameters
  expect_equal(round(test_brandes[["layers"]][[2]][["aes_params"]][["size"]]), 5)
  expect_equal(as.character(test_brandes[["layers"]][[2]][["aes_params"]][["shape"]]), "circle")
})

# Unweighted, signed, undirected network
test_ison_coleman <- autographr(ison_adolescents)

test_that("autographr works for unweighted, signed, undirected networks", {
  # Node position
  expect_equal(round(test_ison_coleman[["data"]][["x"]][[1]]), 1)
  expect_equal(round(test_ison_coleman[["data"]][["y"]][[1]]), 2)
  # Edge parameters
  expect_equal(test_ison_coleman[["layers"]][[1]][["aes_params"]][["edge_alpha"]], 0.4)
  expect_equal(test_ison_coleman[["layers"]][[1]][["aes_params"]][["edge_linetype"]], "solid")
  expect_equal(test_ison_coleman[["layers"]][[1]][["aes_params"]][["edge_colour"]], "black")
  # Node parameters
  expect_equal(round(test_ison_coleman[["layers"]][[2]][["aes_params"]][["size"]]), 5)
  expect_equal(as.character(test_ison_coleman[["layers"]][[2]][["aes_params"]][["shape"]]), "circle")
})

# Test node_measure function with ison_coleman
test_node_measure_max <- autographr(ison_adolescents,
  node_measure = node_betweenness,
  identify_function = max
)
test_node_measure_min <- autographr(ison_adolescents,
  node_measure = node_betweenness,
  identify_function = min
)

test_that("autographr works with node_measure functionality", {
  # Node color is determined by factor levels
  expect_equal(
    rlang::as_label(test_node_measure_max[["layers"]][[2]][["mapping"]][["colour"]]),
    "color_factor"
  )
  expect_equal(
    rlang::as_label(test_node_measure_min[["layers"]][[2]][["mapping"]][["colour"]]),
    "color_factor"
  )
  # Node size
  expect_equal(test_node_measure_max[["layers"]][[2]][["geom"]][["default_aes"]][["size"]], 1.5)
  expect_equal(test_node_measure_min[["layers"]][[2]][["geom"]][["default_aes"]][["size"]], 1.5)
})

# Weighted and directed network: ison_networkers

test_ison_networkers <- autographr(ison_networkers)

test_that("autographr works for unsigned, weighted and directed networks", {
  # Arrows
  expect_equal(test_ison_networkers[["layers"]][[1]][["geom_params"]][["arrow"]][["angle"]], 15)
  # Node size
  expect_equal(test_ison_networkers[["layers"]][[2]][["geom"]][["default_aes"]][["size"]], 1.5)
})

# Bipartite network: mpn_usa_advice

test_usa_advice <- autographr(mpn_elite_usa_advice)

test_that("autographr works for signed, unweighted and undirected bipartite networks", {
  # Names
  expect_equal(test_usa_advice[["data"]][["name"]][[1]], "Albright")
  # Edges
  expect_equal(test_usa_advice[["layers"]][[1]][["aes_params"]][["edge_linetype"]], "solid")
  expect_equal(test_usa_advice[["layers"]][[1]][["aes_params"]][["edge_colour"]], "black")
  expect_equal(test_usa_advice[["layers"]][[1]][["aes_params"]][["edge_alpha"]], 0.4)
  # Nodes
  expect_equal(test_usa_advice[["layers"]][[2]][["aes_params"]][["shape"]][1:3], rep("circle", 3))
  expect_equal(round(test_usa_advice[["layers"]][[2]][["aes_params"]][["size"]]), 1)
  # Labels
  expect_equal(rlang::as_label(test_usa_advice[["layers"]][[3]][["mapping"]][["label"]]), "name")
})

# Testing the node_color, node_size, node_group, and node_shape args by specifying a node attribute
test_that("autographr works for fancy node mods", {
  ison_marvel_relationships <- dplyr::mutate(ison_marvel_relationships, nodesize = Appearances/1000)
  testcolnodes <- autographr(ison_marvel_relationships,
                             node_color = "Gender",
                             node_size = "nodesize",
                             node_shape = "Attractive",
                             node_group = "Rich")
  expect_equal(class(testcolnodes), c("ggraph","gg","ggplot"))
  expect_equal(round(testcolnodes$data$x[1]), 4)
  expect_equal(round(testcolnodes$data$y[1]), 3)
})
