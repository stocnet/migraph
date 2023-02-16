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
test_marvel <- autographr(to_giant(ison_marvel_relationships))

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
  #expect_s3_class(test_algebra[["layers"]][[2]][["aes_params"]][["end_cap"]], "ggraph_geometry")
  # Node parameters
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
  #expect_s3_class(test_networkers[["layers"]][[2]][["aes_params"]][["end_cap"]], "ggraph_geometry")
  expect_equal(rlang::quo_get_expr(test_networkers[["layers"]][[2]][["mapping"]][["edge_width"]]),
               as.name("weight"))
  # Node parameters
  expect_equal(round(test_networkers[["layers"]][[3]][["aes_params"]][["size"]]), 2)
  expect_equal(test_networkers[["layers"]][[3]][["aes_params"]][["shape"]], "circle")
})

# Test node/tie_is_min() and node/tie_is_max() work well with autographr
test_that("autographr works with _min and _max", {
  testplot <- ison_brandes %>% 
    mutate(high_degree = node_is_max(node_degree())) %>% 
    activate(edges) %>% 
    mutate(high_betweenness = tie_is_max(tie_betweenness(ison_brandes))) %>% 
    autographr(node_color = "high_degree", edge_color = "high_betweenness")
  expect_equal(testplot[["plot_env"]][["lo"]][["high_degree"]], node_is_max(node_degree(ison_brandes)))
  expect_equal(rlang::quo_get_expr(testplot[["layers"]][[2]][["mapping"]][["colour"]]),
               as.name("color_factor_node"))
  expect_equal(rlang::quo_get_expr(testplot[["layers"]][[1]][["mapping"]][["edge_colour"]]),
               as.name("edge_color"))
})


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
  # one-mode network
  ison_marvel_relationships <- dplyr::mutate(ison_marvel_relationships, nodesize = Appearances/1000)
  testcolnodes <- autographr(ison_marvel_relationships,
                             node_color = "Gender",
                             node_size = "nodesize",
                             node_shape = "Attractive",
                             node_group = "Rich")
  expect_s3_class(testcolnodes, c("ggraph","gg","ggplot"))
  expect_s3_class(testcolnodes[["plot_env"]][["node_group"]], "factor")
  expect_equal(testcolnodes[["plot_env"]][["node_group"]],
               as.factor(node_attribute(ison_marvel_relationships, "Rich")))
  expect_equal(testcolnodes[["layers"]][[4]][["aes_params"]]$size,
               node_attribute(ison_marvel_relationships, "nodesize"))
  expect_equal(length(unique(testcolnodes[["layers"]][[4]][["aes_params"]]$shape)),
               length(unique(node_attribute(ison_marvel_relationships, "Attractive"))))
  expect_equal(round(testcolnodes$data$x[1]), 4)
  expect_equal(round(testcolnodes$data$y[1]), 3)
  expect_equal(nrow(testcolnodes[["plot_env"]][["lo"]]),
               network_nodes(ison_marvel_relationships))
  # two-mode network
  ison_southern_women <- add_node_attribute(ison_southern_women, "group",
                                            c(node_fast_greedy(ison_southern_women)))
  test2 <- autographr(ison_southern_women,
                      node_color = "type",
                      node_group = "group")
  expect_s3_class(test2, c("ggraph","gg","ggplot"))
  expect_s3_class(test2[["plot_env"]][["node_group"]], "factor")
  expect_equal(test2[["plot_env"]][["node_group"]],
               as.factor(node_attribute(ison_southern_women, "group")))
  expect_equal(round(test2$data$x[1]), 1)
  expect_equal(round(test2$data$y[1]), 1)
  expect_equal(nrow(test2[["plot_env"]][["lo"]]),
               network_nodes(ison_southern_women))
})

test_that("edge colours graph correctly", {
  ison_brandes2 <- migraph::ison_brandes2 %>%
    migraph::add_tie_attribute("tiecolour",
                                c("A", "B", "A", "B", "B", "B", "B", "B", "B", "B", "B", "B"))
  test_brandes2 <- autographr(ison_brandes2, edge_color = "tiecolour")
  expect_false(is.null(test_brandes2$layers[[1]]$mapping$edge_colour))
})

# Named networks
test_that("named networks plot correctly", {
  onemode <- autographr(ison_adolescents)
  twomode <- autographr(ison_southern_women)
  expect_equal(onemode[["data"]][["name"]], node_names(ison_adolescents))
  expect_equal(twomode[["data"]][["name"]], node_names(ison_southern_women))
})

