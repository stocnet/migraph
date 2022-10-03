test_tbl <- as_tidygraph(ison_southern_women)
test_igr <- ison_southern_women
test_mat <- as_matrix(ison_southern_women)

test_that("one mode degree centrality calculated correctly",{
  expect_equal(unname(node_degree(mpn_elite_mex, normalized = FALSE)[1:5]), c(3,6,8,6,6))
})
test_that("one mode strength centrality calculated correctly",{
  expect_equal(unname(node_degree(to_unweighted(ison_networkers), direction = "in", normalized = FALSE)[1:5]), 
               c(29, 24, 11, 18, 8))
  expect_equal(unname(node_degree(ison_networkers, direction = "in", normalized = FALSE, alpha = 1)[1:5]), 
               c(2495, 1212, 101, 322, 89))
})

test_that("two mode degree centrality calculated correctly",{
  expect_equal(unname(node_degree(test_mat, normalized = FALSE)[1:5]), c(8,7,8,7,4))
  expect_equal(unname(node_degree(test_igr, normalized = FALSE)[1:5]), c(8,7,8,7,4))
  expect_equal(unname(with_graph(test_tbl, node_degree(normalized = FALSE))[1:5]), c(8,7,8,7,4))
  expect_equal(unname(node_degree(test_mat, normalized = FALSE)[28:32]), c(6,4,7,4,4))
  expect_equal(unname(node_degree(test_igr, normalized = FALSE)[28:32]), c(6,4,7,4,4))
  expect_equal(unname(with_graph(test_tbl, node_degree(normalized = FALSE))[28:32]), c(6,4,7,4,4))
  expect_equal(unname(round(node_degree(test_mat, normalized = TRUE)[1:5],4)), c(0.5714, .5, .5714, .5, .2857))
  expect_equal(unname(round(node_degree(test_igr, normalized = TRUE)[1:5],4)), c(0.5714, .5, .5714, .5, .2857))
  expect_equal(unname(round(with_graph(test_tbl, node_degree(normalized = TRUE))[1:5],4)), c(0.5714, .5, .5714, .5, .2857))
  expect_equal(unname(round(node_degree(test_mat, normalized = TRUE)[28:32],4)), c(0.3333, .2222, .3889, .2222, .2222))
  expect_equal(unname(round(node_degree(test_igr, normalized = TRUE)[28:32],4)), c(0.3333, .2222, .3889, .2222, .2222))
  expect_equal(unname(round(with_graph(test_tbl, node_degree(normalized = TRUE))[28:32],4)), c(0.3333, .2222, .3889, .2222, .2222))
})

test_that("one mode closeness centrality calculated correctly",{
  expect_equal(round(unname(node_closeness(mpn_elite_mex, normalized = FALSE)[1:3]),2), c(0.01, 0.01, 0.01))
})

test_that("two mode closeness centrality calculated correctly",{
  expect_equal(unname(round(node_closeness(test_mat, normalized = FALSE)[1:5], 4)), c(0.0167, 0.0152, 0.0167, 0.0152, 0.0125))
  expect_equal(unname(round(node_closeness(test_igr, normalized = FALSE)[1:5], 4)), c(0.0167, 0.0152, 0.0167, 0.0152, 0.0125))
  expect_equal(unname(round(with_graph(test_tbl, node_closeness(normalized = FALSE))[1:5], 4)), c(0.0167, 0.0152, 0.0167, 0.0152, 0.0125))
  expect_equal(unname(round(node_closeness(test_mat, normalized = FALSE)[28:32], 4)), c(0.0128, 0.0122, 0.0132, 0.0122, 0.0122))
  expect_equal(unname(round(node_closeness(test_igr, normalized = FALSE)[28:32], 4)), c(0.0128, 0.0122, 0.0132, 0.0122, 0.0122))
  expect_equal(unname(round(with_graph(test_tbl, node_closeness(normalized = FALSE))[28:32], 4)), c(0.0128, 0.0122, 0.0132, 0.0122, 0.0122))
  expect_equal(unname(round(node_closeness(test_mat, normalized = TRUE)[1:5],4)), c(0.8000, 0.7273, 0.8000, 0.7273, 0.6000))
  expect_equal(unname(round(node_closeness(test_igr, normalized = TRUE)[1:5],4)), c(0.8000, 0.7273, 0.8000, 0.7273, 0.6000))
  expect_equal(unname(round(with_graph(test_tbl, node_closeness(normalized = TRUE))[1:5],4)), c(0.8000, 0.7273, 0.8000, 0.7273, 0.6000))
  expect_equal(unname(round(node_closeness(test_mat, normalized = TRUE)[28:32],4)), c(0.5641, 0.5366, 0.5789, 0.5366, 0.5366))
  expect_equal(unname(round(node_closeness(test_igr, normalized = TRUE)[28:32],4)), c(0.5641, 0.5366, 0.5789, 0.5366, 0.5366))
  expect_equal(unname(round(with_graph(test_tbl, node_closeness(normalized = TRUE))[28:32],4)), c(0.5641, 0.5366, 0.5789, 0.5366, 0.5366))
})

test_that("one mode betweenness centrality calculated correctly",{
  expect_equal(round(unname(node_betweenness(mpn_elite_mex, normalized = FALSE)[1:3]),2), c(2.83, 4.59, 17.36))
})

test_that("two mode betweenness centrality calculated correctly",{
  expect_equal(unname(node_betweenness(test_mat, normalized = FALSE)[1:5]), c(42.759998, 22.856540, 38.739264, 22.011910, 4.727942))
  expect_equal(unname(node_betweenness(test_igr, normalized = FALSE)[1:5]), c(42.759998, 22.856540, 38.739264, 22.011910, 4.727942))
  expect_equal(unname(with_graph(test_tbl, node_betweenness(normalized = FALSE))[1:5]), c(42.759998, 22.856540, 38.739264, 22.011910, 4.727942))
  expect_equal(unname(round(node_betweenness(test_mat, normalized = FALSE)[28:32],2)), c(6.82, 9.02, 10.24, 1.89, 1.89))
  expect_equal(unname(round(node_betweenness(test_igr, normalized = FALSE)[28:32],2)), c(6.82, 9.02, 10.24, 1.89, 1.89))
  expect_equal(unname(round(with_graph(test_tbl, node_betweenness(normalized = FALSE))[28:32],2)), c(6.82, 9.02, 10.24, 1.89, 1.89))
  expect_equal(unname(round(node_betweenness(test_mat, normalized = TRUE)[1:5],4)), c(0.0967, 0.0517, 0.0876, 0.0498, 0.0107))
  expect_equal(unname(round(node_betweenness(test_igr, normalized = TRUE)[1:5],4)), c(0.0967, 0.0517, 0.0876, 0.0498, 0.0107))
  expect_equal(unname(round(with_graph(test_tbl, node_betweenness(normalized = TRUE))[1:5],4)), c(0.0967, 0.0517, 0.0876, 0.0498, 0.0107))
  expect_equal(unname(round(node_betweenness(test_mat, normalized = TRUE)[28:32],4)), c(0.0151, 0.02, 0.0226, 0.0042, 0.0042))
  expect_equal(unname(round(node_betweenness(test_igr, normalized = TRUE)[28:32],4)), c(0.0151, 0.02, 0.0226, 0.0042, 0.0042))
  expect_equal(unname(round(with_graph(test_tbl, node_betweenness(normalized = TRUE))[28:32],4)), c(0.0151, 0.02, 0.0226, 0.0042, 0.0042))
})


test_that("one mode eigenvector centrality calculated correctly",{
  expect_equal(round(unname(node_eigenvector(mpn_elite_mex, normalized = FALSE)[1:3]),2), c(0.06, 0.08, 0.12))
  expect_equal(round(unname(node_eigenvector(mpn_elite_mex, normalized = TRUE)[1:3]),2), 
               c(0.08, 0.11, 0.17))
})

test_that("two mode eigenvector centrality calculated correctly",{
  expect_equal(unname(round(node_eigenvector(test_mat, normalized = FALSE)[1:3], 2)), c(0.30, 0.28, 0.33))
  expect_equal(unname(round(node_eigenvector(test_igr, normalized = FALSE)[1:3], 2)), c(0.30, 0.28, 0.33))
  expect_equal(unname(round(node_eigenvector(test_mat, normalized = FALSE)[30:32], 2)), c(0.26, 0.18, 0.18))
  expect_equal(unname(round(node_eigenvector(test_igr, normalized = FALSE)[30:32], 2)), c(0.26, 0.18, 0.18))
  expect_equal(unname(round(node_eigenvector(test_igr, normalized = TRUE)[1:3], 2)), c(0.42, 0.40, 0.47))
})

test_that("node measure class works", {
  expect_s3_class(node_degree(ison_algebra), "node_measure")
  expect_s3_class(node_betweenness(ison_algebra), "node_measure")
  expect_s3_class(node_closeness(ison_algebra), "node_measure")
  expect_s3_class(node_eigenvector(ison_algebra), "node_measure")
  expect_s3_class(node_reach(ison_algebra), "node_measure")
  testplot <- plot(node_degree(ison_algebra))
  expect_equal(testplot$data$Score, unname(node_degree(ison_algebra)))
  expect_equal(testplot$labels$y, "Frequency")
})

####### Centralization

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

####### Edge centrality
test_that("tie_degree works", {
  expect_s3_class(tie_degree(ison_adolescents), 
                  "tie_measure")
  expect_length(tie_degree(ison_adolescents), 
                network_ties(ison_adolescents))
})

test_that("tie_betweenness works", {
  expect_s3_class(tie_betweenness(ison_adolescents), 
                  "tie_measure")
  expect_length(tie_betweenness(ison_adolescents), 
                network_ties(ison_adolescents))
  expect_equal(unname(tie_betweenness(ison_adolescents)[1:3]), 
               c(7,3,5), tolerance = 0.001)
})

test_that("tie_closeness works", {
  expect_s3_class(tie_closeness(ison_adolescents), 
                  "tie_measure")
  expect_length(tie_closeness(ison_adolescents), 
                network_ties(ison_adolescents))
  expect_equal(unname(tie_closeness(ison_adolescents)[1:3]), 
               c(0.562,0.692,0.600), tolerance = 0.001)
})

test_that("tie_eigenvector works", {
  expect_s3_class(tie_eigenvector(ison_southern_women), 
                  "tie_measure")
  expect_length(tie_eigenvector(ison_southern_women), 
                network_ties(ison_southern_women))
})
