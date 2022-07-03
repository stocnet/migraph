# Unweighted, unsigned, directed network
test_brandes <- autographr(migraph::ison_brandes)

test_that("autographr works for unweighted, unsigned, directed networks", {
  # Node position
  expect_equal(round(test_brandes[["data"]][["x"]][[1]]), 3)
  expect_equal(round(test_brandes[["data"]][["y"]][[1]]), -1)
  # Edge parameters
  expect_equal(test_brandes[["layers"]][[1]][["aes_params"]][["edge_alpha"]], 0.4)
  expect_equal(test_brandes[["layers"]][[1]][["aes_params"]][["edge_linetype"]], "solid")
  expect_equal(rlang::quo_get_expr(test_brandes[["layers"]][[1]][["mapping"]][["edge_colour"]]),
               as.name("edge_color"))
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
  expect_equal(rlang::quo_get_expr(test_ison_coleman[["layers"]][[1]][["mapping"]][["edge_colour"]]),
               as.name("edge_color"))
  # Node parameters
  expect_equal(round(test_ison_coleman[["layers"]][[2]][["aes_params"]][["size"]]), 5)
  expect_equal(as.character(test_ison_coleman[["layers"]][[2]][["aes_params"]][["shape"]]), "circle")
})

# Test node_measure function with ison_coleman
# test_node_measure_max <- autographr(ison_adolescents,
#   highlight_measure = "node_betweenness",
#   identify_function = "max"
# )
# test_node_measure_min <- autographr(ison_adolescents,
#   highlight_measure = "node_betweenness",
#   identify_function = "min"
# )
# test_node_measure_max_edge_measure_max <- autographr(ison_adolescents,
#                                     highlight_measure = c("node_betweenness", "tie_betweenness"),
#                                     identify_function = c("max", "max")
# )
# 
# test_that("autographr works with node_measure functionality", {
#   # Node color is determined by factor levels
#   expect_equal(
#     rlang::as_label(test_node_measure_max[["layers"]][[2]][["mapping"]][["colour"]]),
#     "color_factor_node"
#   )
#   expect_equal(
#     rlang::as_label(test_node_measure_min[["layers"]][[2]][["mapping"]][["colour"]]),
#     "color_factor_node"
#   )
#   # Node size
#   expect_equal(test_node_measure_max[["layers"]][[2]][["geom"]][["default_aes"]][["size"]], 1.5)
#   expect_equal(test_node_measure_min[["layers"]][[2]][["geom"]][["default_aes"]][["size"]], 1.5)
# })

# Test edge_measure function with ison_coleman
# test_edge_measure_max <- autographr(ison_adolescents,
#                                     highlight_measure = "tie_betweenness",
#                                     identify_function = "max"
# )
# test_edge_measure_min <- autographr(ison_adolescents,
#                                     highlight_measure = "tie_betweenness",
#                                     identify_function = "min"
# )
# 
# test_that("autographr works with tie_measure functionality", {
#   # Node color is determined by factor levels
#   expect_equal(
#     rlang::as_label(rlang::quo_get_expr(test_edge_measure_max[["layers"]][[1]][["mapping"]][["edge_color"]])),
#     "edge_color"
#   )
#   expect_equal(
#     rlang::as_label(rlang::quo_get_expr(test_edge_measure_min[["layers"]][[1]][["mapping"]][["edge_color"]])),
#     "edge_color"
#   )
#   # Node size
#   expect_equal(test_edge_measure_max[["layers"]][[2]][["geom"]][["default_aes"]][["size"]], 1.5)
#   expect_equal(test_edge_measure_min[["layers"]][[2]][["geom"]][["default_aes"]][["size"]], 1.5)
# })

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
  expect_equal(rlang::quo_get_expr(test_usa_advice[["layers"]][[1]][["mapping"]][["edge_colour"]]),
               as.name("edge_color"))
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

test_that("autographr works with edge colours", {
  ison_brandes2 <- migraph::ison_brandes2 %>%
    migraph::add_tie_attribute("tiecolour",
                                c("A", "B", "A", "B", "B", "B", "B", "B", "B", "B", "B", "B"))
  test_brandes2 <- autographr(ison_brandes2, edge_color = "tiecolour")
  expect_equal(length(test_brandes2[["plot_env"]][["edge_color"]]),
               graph_ties(ison_brandes2))
})
