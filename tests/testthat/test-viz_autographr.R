# Unweighted, unsigned, undirected network
test_brandes <- autographr(migraph::ison_brandes)

test_that("unweighted, unsigned, undirected networks graph correctly", {
  # Node position
  expect_equal(round(test_brandes[["data"]][["x"]][[1]]), 3)
  expect_equal(round(test_brandes[["data"]][["y"]][[1]]), -1)
  # Edge parameters
  expect_equal(test_brandes[["layers"]][[1]][["aes_params"]][["edge_alpha"]], 0.4)
  expect_equal(test_brandes[["layers"]][[1]][["aes_params"]][["edge_linetype"]], "solid")
  expect_equal(rlang::quo_get_expr(test_brandes[["layers"]][[1]][["mapping"]][["edge_colour"]]),
               as.name("edge_color"))
  # Node parameters
  expect_equal(round(test_brandes[["layers"]][[2]][["aes_params"]][["size"]]), 5)
  expect_equal(as.character(test_brandes[["layers"]][[2]][["aes_params"]][["shape"]]), "circle")
})

# Unweighted, signed, undirected network
test_marvel <- autographr(to_main_component(ison_marvel_relationships))

test_that("unweighted, signed, undirected networks graph correctly", {
  # Node position
  expect_equal(round(test_marvel[["data"]][["x"]][[1]]), -1)
  expect_equal(round(test_marvel[["data"]][["y"]][[1]]), 1)
  # Edge parameters
  expect_equal(test_marvel[["layers"]][[2]][["aes_params"]][["edge_alpha"]], 0.4)
  expect_equal(rlang::quo_get_expr(test_marvel[["layers"]][[2]][["mapping"]][["edge_linetype"]]),
               as.name("edge_linetype"))
  # Node parameters
  expect_equal(test_marvel[["layers"]][[3]][["aes_params"]][["size"]], 1)
  expect_equal(test_marvel[["layers"]][[3]][["aes_params"]][["shape"]], "circle")
})

# Unweighted, unsigned, directed network
test_algebra <- autographr(ison_algebra)

test_that("unweighted, unsigned, directed networks graph correctly", {
  # Node position
  expect_equal(round(test_algebra[["data"]][["x"]][[1]]), 0)
  expect_equal(round(test_algebra[["data"]][["y"]][[1]]), 0)
  # Edge parameters
  expect_equal(test_algebra[["layers"]][[2]][["aes_params"]][["edge_alpha"]], 0.4)
  expect_equal(test_algebra[["layers"]][[2]][["aes_params"]][["edge_linetype"]], "solid")
  expect_equal(test_algebra[["layers"]][[2]][["aes_params"]][["edge_colour"]], "black")
  expect_equal(as.character(test_algebra[["layers"]][[2]][["aes_params"]][["end_cap"]]), "circle")
  expect_s3_class(test_algebra[["layers"]][[2]][["aes_params"]][["end_cap"]], "geometry")
  # # Node parameters
  expect_equal(round(test_algebra[["layers"]][[3]][["aes_params"]][["size"]]), 3)
  expect_equal(test_algebra[["layers"]][[3]][["aes_params"]][["shape"]], "circle")
})

# Weighted, unsigned, directed network
test_networkers <- autographr(ison_networkers)

test_that("weighted, unsigned, directed networks graph correctly", {
  # Node position
  expect_equal(round(test_networkers[["data"]][["x"]][[1]]), 0)
  expect_equal(round(test_networkers[["data"]][["y"]][[1]]), 0)
  # Edge parameters
  expect_equal(test_networkers[["layers"]][[2]][["aes_params"]][["edge_alpha"]], 0.4)
  expect_equal(test_networkers[["layers"]][[2]][["aes_params"]][["edge_linetype"]], "solid")
  expect_equal(test_networkers[["layers"]][[2]][["aes_params"]][["edge_colour"]], "black")
  expect_equal(as.character(test_networkers[["layers"]][[2]][["aes_params"]][["end_cap"]]), "circle")
  expect_s3_class(test_networkers[["layers"]][[2]][["aes_params"]][["end_cap"]], "geometry")
  expect_equal(rlang::quo_get_expr(test_networkers[["layers"]][[2]][["computed_mapping"]][["edge_width"]]),
               as.name("weight"))
  # # Node parameters
  expect_equal(round(test_networkers[["layers"]][[3]][["aes_params"]][["size"]]), 2)
  expect_equal(test_networkers[["layers"]][[3]][["aes_params"]][["shape"]], "circle")
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
# 
# Bipartite network: mpn_usa_advice

test_usa_advice <- autographr(mpn_elite_usa_advice)

test_that("autographr works for bipartite networks", {
  # Names
  expect_equal(test_usa_advice[["data"]][["name"]][[1]], "Albright")
  # Labels
  expect_equal(rlang::as_label(test_usa_advice[["layers"]][[1]][["mapping"]][["label"]]), "name")
  # Edges
  expect_equal(test_usa_advice[["layers"]][[2]][["aes_params"]][["edge_linetype"]], "solid")
  expect_equal(rlang::quo_get_expr(test_usa_advice[["layers"]][[2]][["mapping"]][["edge_colour"]]),
               as.name("edge_color"))
  expect_equal(test_usa_advice[["layers"]][[2]][["aes_params"]][["edge_alpha"]], 0.4)
  # Nodes
  expect_equal(levels(as.factor(test_usa_advice[["layers"]][[3]][["aes_params"]][["shape"]])),
               c("circle", "square"))
  expect_equal(round(test_usa_advice[["layers"]][[3]][["aes_params"]][["size"]]), 1)
})

# Testing the node_color, node_size, node_group, and node_shape args by specifying a node attribute
test_that("fancy node mods graph correctly", {
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

test_that("edge colours graph correctly", {
  ison_brandes2 <- migraph::ison_brandes2 %>%
    migraph::add_tie_attribute("tiecolour",
                                c("A", "B", "A", "B", "B", "B", "B", "B", "B", "B", "B", "B"))
  test_brandes2 <- autographr(ison_brandes2, edge_color = "tiecolour")
  expect_false(is.null(test_brandes2$layers[[1]]$mapping$edge_colour))
})
