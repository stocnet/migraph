#*************** Test the diversity family of functions ***********************#

marvelsubgraph <- ison_marvel_relationships %>%
  mutate(wt_group = igraph::membership(igraph::cluster_walktrap(ison_marvel_relationships)))


test_that("Blau index function works", {
  expect_equal(round(graph_blau_index(ison_marvel_relationships, "Gender"), 2), 0.31)
  expect_equal(lapply(graph_blau_index(marvelsubgraph, "Gender", "wt_group"), round, 2),
               list(`Cluster 1` = 0.19, `Cluster 2` = 0.44,
                    `Cluster 3` = 0, `Cluster 4` = 0, `Cluster 5` = 0) )
})

test_that("EI index function works", {
expect_equal(round(graph_ei_index(ison_marvel_relationships, "Gender"), 2), -0.39)
})
