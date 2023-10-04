test_tbl <- manynet::as_tidygraph(manynet::ison_southern_women)
test_igr <- manynet::ison_southern_women
test_mat <- manynet::as_matrix(manynet::ison_southern_women)

test_that("one mode degree centrality calculated correctly",{
  expect_equal(top5(node_degree(mpn_elite_mex, normalized = FALSE)), c(3,6,8,6,6))
})
test_that("one mode strength centrality calculated correctly",{
  expect_equal(top5(node_degree(to_unweighted(ison_networkers), direction = "in", normalized = FALSE)), 
               c(29, 24, 11, 18, 8))
  expect_equal(top5(node_degree(ison_networkers, direction = "in", normalized = FALSE, alpha = 1)), 
               c(2495, 1212, 101, 322, 89))
})

test_that("two mode degree centrality calculated correctly",{
  expect_equal(top5(node_degree(test_mat, normalized = FALSE)), c(8,7,8,7,4))
  expect_equal(top5(node_degree(test_igr, normalized = FALSE)), c(8,7,8,7,4))
  expect_equal(top5(with_graph(test_tbl, node_degree(normalized = FALSE))), c(8,7,8,7,4))
  expect_equal(bot5(node_degree(test_mat, normalized = FALSE)), c(6,4,7,4,4))
  expect_equal(bot5(node_degree(test_igr, normalized = FALSE)), c(6,4,7,4,4))
  expect_equal(bot5(with_graph(test_tbl, node_degree(normalized = FALSE))), c(6,4,7,4,4))
  expect_equal(top5(node_degree(test_mat, normalized = TRUE)), c(0.5714, .5, .5714, .5, .2857))
  expect_equal(top5(node_degree(test_igr, normalized = TRUE)), c(0.5714, .5, .5714, .5, .2857))
  expect_equal(top5(with_graph(test_tbl, node_degree(normalized = TRUE))), c(0.5714, .5, .5714, .5, .2857))
  expect_equal(bot5(node_degree(test_mat, normalized = TRUE)), c(0.3333, .2222, .3889, .2222, .2222))
  expect_equal(bot5(node_degree(test_igr, normalized = TRUE)), c(0.3333, .2222, .3889, .2222, .2222))
  expect_equal(bot5(with_graph(test_tbl, node_degree(normalized = TRUE))), c(0.3333, .2222, .3889, .2222, .2222))
})

test_that("one mode closeness centrality calculated correctly",{
  expect_equal(top3(node_closeness(mpn_elite_mex, normalized = FALSE)), c(0.0118, 0.0119, 0.0137))
})

test_that("two mode closeness centrality calculated correctly",{
  expect_equal(top5(node_closeness(test_mat, normalized = FALSE)), c(0.0167, 0.0152, 0.0167, 0.0152, 0.0125))
  expect_equal(top5(node_closeness(test_igr, normalized = FALSE)), c(0.0167, 0.0152, 0.0167, 0.0152, 0.0125))
  expect_equal(top5(with_graph(test_tbl, node_closeness(normalized = FALSE))), c(0.0167, 0.0152, 0.0167, 0.0152, 0.0125))
  expect_equal(bot5(node_closeness(test_mat, normalized = FALSE)), c(0.0128, 0.0122, 0.0132, 0.0122, 0.0122))
  expect_equal(bot5(node_closeness(test_igr, normalized = FALSE)), c(0.0128, 0.0122, 0.0132, 0.0122, 0.0122))
  expect_equal(bot5(with_graph(test_tbl, node_closeness(normalized = FALSE))), c(0.0128, 0.0122, 0.0132, 0.0122, 0.0122))
  expect_equal(top5(node_closeness(test_mat, normalized = TRUE)), c(0.8000, 0.7273, 0.8000, 0.7273, 0.6000))
  expect_equal(top5(node_closeness(test_igr, normalized = TRUE)), c(0.8000, 0.7273, 0.8000, 0.7273, 0.6000))
  expect_equal(top5(with_graph(test_tbl, node_closeness(normalized = TRUE))), c(0.8000, 0.7273, 0.8000, 0.7273, 0.6000))
  expect_equal(bot5(node_closeness(test_mat, normalized = TRUE)), c(0.5641, 0.5366, 0.5789, 0.5366, 0.5366))
  expect_equal(bot5(node_closeness(test_igr, normalized = TRUE)), c(0.5641, 0.5366, 0.5789, 0.5366, 0.5366))
  expect_equal(bot5(with_graph(test_tbl, node_closeness(normalized = TRUE))), c(0.5641, 0.5366, 0.5789, 0.5366, 0.5366))
})

test_that("one mode betweenness centrality calculated correctly",{
  expect_equal(top3(node_betweenness(mpn_elite_mex, normalized = FALSE)), c(2.8345, 4.5922, 17.3583))
})

test_that("two mode betweenness centrality calculated correctly",{
  expect_equal(top5(node_betweenness(test_mat, normalized = FALSE)), c(42.7600, 22.8565, 38.7393, 22.0119, 4.7279))
  expect_equal(top5(node_betweenness(test_igr, normalized = FALSE)), c(42.7600, 22.8565, 38.7393, 22.0119, 4.7279))
  expect_equal(top5(with_graph(test_tbl, node_betweenness(normalized = FALSE))), c(42.7600, 22.8565, 38.7393, 22.0119, 4.7279))
  expect_equal(bot5(node_betweenness(test_mat, normalized = FALSE)), c(6.8186, 9.0194, 10.2354, 1.8892, 1.8892))
  expect_equal(bot5(node_betweenness(test_igr, normalized = FALSE)), c(6.8186, 9.0194, 10.2354, 1.8892, 1.8892))
  expect_equal(bot5(with_graph(test_tbl, node_betweenness(normalized = FALSE))), c(6.8186, 9.0194, 10.2354, 1.8892, 1.8892))
  expect_equal(top5(node_betweenness(test_mat, normalized = TRUE),4), c(0.0967, 0.0517, 0.0876, 0.0498, 0.0107))
  expect_equal(top5(node_betweenness(test_igr, normalized = TRUE),4), c(0.0967, 0.0517, 0.0876, 0.0498, 0.0107))
  expect_equal(top5(with_graph(test_tbl, node_betweenness(normalized = TRUE)),4), c(0.0967, 0.0517, 0.0876, 0.0498, 0.0107))
  expect_equal(bot5(node_betweenness(test_mat, normalized = TRUE),4), c(0.0151, 0.02, 0.0226, 0.0042, 0.0042))
  expect_equal(bot5(node_betweenness(test_igr, normalized = TRUE),4), c(0.0151, 0.02, 0.0226, 0.0042, 0.0042))
  expect_equal(bot5(with_graph(test_tbl, node_betweenness(normalized = TRUE)),4), c(0.0151, 0.02, 0.0226, 0.0042, 0.0042))
})

test_that("one mode eigenvector centrality calculated correctly",{
  expect_equal(top3(node_eigenvector(mpn_elite_mex, normalized = FALSE)), c(0.0571, 0.0771, 0.1176))
  expect_equal(top3(node_eigenvector(mpn_elite_mex, normalized = TRUE)), c(0.0808, 0.1090, 0.1663))
})

test_that("two mode eigenvector centrality calculated correctly",{
  expect_equal(top3(node_eigenvector(test_mat, normalized = FALSE)), c(0.2991, 0.2809, 0.3338))
  expect_equal(top3(node_eigenvector(test_igr, normalized = FALSE)), c(0.2991, 0.2809, 0.3338))
  expect_equal(bot3(node_eigenvector(test_mat, normalized = FALSE)), c(0.2551, 0.1774, 0.1774))
  expect_equal(bot3(node_eigenvector(test_igr, normalized = FALSE)), c(0.2551, 0.1774, 0.1774))
  expect_equal(top3(node_eigenvector(test_igr, normalized = TRUE)), c(0.423, 0.3973, 0.4721))
})

test_that("node measure class works", {
  expect_s3_class(node_degree(ison_algebra), "node_measure")
  expect_s3_class(node_betweenness(ison_algebra), "node_measure")
  expect_s3_class(node_closeness(ison_algebra), "node_measure")
  expect_s3_class(node_eigenvector(ison_algebra), "node_measure")
  expect_s3_class(node_reach(ison_algebra), "node_measure")
  testplot <- plot(node_degree(ison_algebra))
  expect_equal(testplot$data$Score, unname(node_degree(ison_algebra)))
  # expect_equal(testplot$labels$y, "Frequency")
})

# ####### Centralization

test_that("one-mode centralisation is calculated correctly", {
  expect_equal(as.numeric(network_degree(mpn_elite_mex)), 0.303, tolerance = 0.001)
  expect_equal(as.numeric(network_closeness(mpn_elite_mex)), 0.386, tolerance = 0.001)
  expect_equal(as.numeric(network_betweenness(mpn_elite_mex)), 0.202, tolerance = 0.001)
  expect_equal(as.numeric(network_eigenvector(mpn_elite_mex)), 0.630, tolerance = 0.001)
})

test_that("two mode degree centralisation calculated correctly", {
  expect_equal(as.numeric(network_degree(ison_southern_women, normalized = FALSE)), c(0.1813, 0.5097), tolerance = 0.001)
  expect_equal(as.numeric(network_degree(ison_southern_women, direction = "in")), c(0.2308, 0.4661), tolerance = 0.001)
  expect_equal(as.numeric(network_degree(ison_southern_women, normalized = TRUE)), c(0.2268, 0.4744), tolerance = 0.001)
})

test_that("two mode closeness centralisation calculated correctly", {
  expect_equal(as.numeric(network_closeness(ison_southern_women, normalized = TRUE)), c(0.2843, 0.4418), tolerance = 0.001)
  expect_equal(as.numeric(network_closeness(ison_southern_women, direction = "in")), c(0.2135, 0.5285), tolerance = 0.001)
})

test_that("two mode betweenness centralisation calculated correctly", {
  expect_equal(as.numeric(network_betweenness(ison_southern_women, normalized = FALSE)), c(0.0580, 0.2073), tolerance = 0.001)
  expect_equal(as.numeric(network_betweenness(ison_southern_women, direction = "in")), c(0.0668, 0.1982), tolerance = 0.001)
  expect_equal(as.numeric(network_betweenness(ison_southern_women, normalized = TRUE)), c(0.0586, 0.207), tolerance = 0.001)
})

test_that("network_measure class works", {
  expect_s3_class(network_degree(ison_algebra), "network_measure")
  expect_s3_class(network_betweenness(mpn_elite_usa_advice), "network_measure")
  expect_s3_class(network_closeness(mpn_elite_usa_advice), "network_measure")
  expect_output(print(network_degree(ison_algebra)))
})

# ####### Edge centrality
test_that("tie_degree works", {
  expect_s3_class(tie_degree(ison_adolescents),
                  "tie_measure")
  expect_length(tie_degree(ison_adolescents),
                manynet::network_ties(ison_adolescents))
})

test_that("tie_betweenness works", {
  expect_s3_class(tie_betweenness(ison_adolescents),
                  "tie_measure")
  expect_length(tie_betweenness(ison_adolescents),
                manynet::network_ties(ison_adolescents))
  expect_equal(top3(tie_betweenness(ison_adolescents)),
               c(7,3,5), tolerance = 0.001)
})

test_that("tie_closeness works", {
  expect_s3_class(tie_closeness(ison_adolescents),
                  "tie_measure")
  expect_length(tie_closeness(ison_adolescents),
                manynet::network_ties(ison_adolescents))
  expect_equal(top3(tie_closeness(ison_adolescents)),
               c(0.562,0.692,0.600), tolerance = 0.001)
})

test_that("tie_eigenvector works", {
  expect_s3_class(tie_eigenvector(ison_southern_women),
                  "tie_measure")
  expect_length(tie_eigenvector(ison_southern_women),
                manynet::network_ties(ison_southern_women))
})
