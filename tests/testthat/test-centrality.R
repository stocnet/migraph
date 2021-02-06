test_tbl <- as_tidygraph(southern_women)
test_igr <- southern_women
test_mat <- as_matrix(southern_women)

test_that("one mode degree centrality calculated correctly",{
  expect_equal(unname(node_degree(mpn_elite_mex)[1:5]), c(2,5,2,3,5))
})

test_that("two mode degree centrality calculated correctly",{
  expect_equal(unname(node_degree(test_mat)[1:5]), c(8,7,8,7,4))
  expect_equal(unname(node_degree(test_igr)[1:5]), c(8,7,8,7,4))
  expect_equal(unname(with_graph(test_tbl, node_degree())[1:5]), c(8,7,8,7,4))
  expect_equal(unname(node_degree(test_mat)[28:32]), c(6,4,7,4,4))
  expect_equal(unname(node_degree(test_igr)[28:32]), c(6,4,7,4,4))
  expect_equal(unname(with_graph(test_tbl, node_degree())[28:32]), c(6,4,7,4,4))
  expect_equal(unname(round(node_degree(test_mat, normalized = TRUE)[1:5],4)), c(0.5714, .5, .5714, .5, .2857))
  expect_equal(unname(round(node_degree(test_igr, normalized = TRUE)[1:5],4)), c(0.5714, .5, .5714, .5, .2857))
  expect_equal(unname(round(with_graph(test_tbl, node_degree(normalized = TRUE))[1:5],4)), c(0.5714, .5, .5714, .5, .2857))
  expect_equal(unname(round(node_degree(test_mat, normalized = TRUE)[28:32],4)), c(0.3333, .2222, .3889, .2222, .2222))
  expect_equal(unname(round(node_degree(test_igr, normalized = TRUE)[28:32],4)), c(0.3333, .2222, .3889, .2222, .2222))
  expect_equal(unname(round(with_graph(test_tbl, node_degree(normalized = TRUE))[28:32],4)), c(0.3333, .2222, .3889, .2222, .2222))
})

test_that("one mode closeness centrality calculated correctly",{
  expect_equal(round(unname(node_closeness(mpn_elite_mex)[1:3]),2), c(0.05, 0.07, 0.05))
})

test_that("two mode closeness centrality calculated correctly",{
  expect_equal(unname(round(node_closeness(test_mat)[1:5], 4)), c(0.0167, 0.0152, 0.0167, 0.0152, 0.0125))
  expect_equal(unname(round(node_closeness(test_igr)[1:5], 4)), c(0.0167, 0.0152, 0.0167, 0.0152, 0.0125))
  expect_equal(unname(round(with_graph(test_tbl, node_closeness())[1:5], 4)), c(0.0167, 0.0152, 0.0167, 0.0152, 0.0125))
  expect_equal(unname(round(node_closeness(test_mat)[28:32], 4)), c(0.0128, 0.0122, 0.0132, 0.0122, 0.0122))
  expect_equal(unname(round(node_closeness(test_igr)[28:32], 4)), c(0.0128, 0.0122, 0.0132, 0.0122, 0.0122))
  expect_equal(unname(round(with_graph(test_tbl, node_closeness())[28:32], 4)), c(0.0128, 0.0122, 0.0132, 0.0122, 0.0122))
  expect_equal(unname(round(node_closeness(test_mat, normalized = TRUE)[1:5],4)), c(0.8000, 0.7273, 0.8000, 0.7273, 0.6000))
  expect_equal(unname(round(node_closeness(test_igr, normalized = TRUE)[1:5],4)), c(0.8000, 0.7273, 0.8000, 0.7273, 0.6000))
  expect_equal(unname(round(with_graph(test_tbl, node_closeness(normalized = TRUE))[1:5],4)), c(0.8000, 0.7273, 0.8000, 0.7273, 0.6000))
  expect_equal(unname(round(node_closeness(test_mat, normalized = TRUE)[28:32],4)), c(0.5641, 0.5366, 0.5789, 0.5366, 0.5366))
  expect_equal(unname(round(node_closeness(test_igr, normalized = TRUE)[28:32],4)), c(0.5641, 0.5366, 0.5789, 0.5366, 0.5366))
  expect_equal(unname(round(with_graph(test_tbl, node_closeness(normalized = TRUE))[28:32],4)), c(0.5641, 0.5366, 0.5789, 0.5366, 0.5366))
})

test_that("one mode betweenness centrality calculated correctly",{
  expect_equal(round(unname(node_betweenness(mpn_elite_mex)[1:3]),2), c(0.00, 13.33, 0.00))
})

test_that("two mode betweenness centrality calculated correctly",{
  expect_equal(unname(node_betweenness(test_mat)[1:5]), c(42.759998, 22.856540, 38.739264, 22.011910, 4.727942))
  expect_equal(unname(node_betweenness(test_igr)[1:5]), c(42.759998, 22.856540, 38.739264, 22.011910, 4.727942))
  expect_equal(unname(with_graph(test_tbl, node_betweenness())[1:5]), c(42.759998, 22.856540, 38.739264, 22.011910, 4.727942))
  expect_equal(unname(round(node_betweenness(test_mat)[28:32],2)), c(6.82, 9.02, 10.24, 1.89, 1.89))
  expect_equal(unname(round(node_betweenness(test_igr)[28:32],2)), c(6.82, 9.02, 10.24, 1.89, 1.89))
  expect_equal(unname(round(with_graph(test_tbl, node_betweenness())[28:32],2)), c(6.82, 9.02, 10.24, 1.89, 1.89))
  expect_equal(unname(round(node_betweenness(test_mat, normalized = TRUE)[1:5],4)), c(0.0967, 0.0517, 0.0876, 0.0498, 0.0107))
  expect_equal(unname(round(node_betweenness(test_igr, normalized = TRUE)[1:5],4)), c(0.0967, 0.0517, 0.0876, 0.0498, 0.0107))
  expect_equal(unname(round(with_graph(test_tbl, node_betweenness(normalized = TRUE))[1:5],4)), c(0.0967, 0.0517, 0.0876, 0.0498, 0.0107))
  expect_equal(unname(round(node_betweenness(test_mat, normalized = TRUE)[28:32],4)), c(0.0151, 0.02, 0.0226, 0.0042, 0.0042))
  expect_equal(unname(round(node_betweenness(test_igr, normalized = TRUE)[28:32],4)), c(0.0151, 0.02, 0.0226, 0.0042, 0.0042))
  expect_equal(unname(round(with_graph(test_tbl, node_betweenness(normalized = TRUE))[28:32],4)), c(0.0151, 0.02, 0.0226, 0.0042, 0.0042))
})


test_that("one mode eigenvector centrality calculated correctly",{
  expect_equal(round(unname(node_eigenvector(mpn_elite_mex)[1:3]),2), c(0.16, 0.34, 0.18))
  expect_equal(round(unname(node_eigenvector(mpn_elite_mex, normalized = TRUE)[1:3]),2), c(0.22, 0.49, 0.25))
})

test_that("two mode eigenvector centrality calculated correctly",{
  expect_equal(unname(round(node_eigenvector(test_mat)[1:3], 2)), c(0.30, 0.28, 0.33))
  expect_equal(unname(round(node_eigenvector(test_igr)[1:3], 2)), c(0.30, 0.28, 0.33))
  expect_equal(unname(round(node_eigenvector(test_mat)[30:32], 2)), c(0.26, 0.18, 0.18))
  expect_equal(unname(round(node_eigenvector(test_igr)[30:32], 2)), c(0.26, 0.18, 0.18))
  expect_error(node_eigenvector(test_igr, normalized = TRUE), "not currently implemented")
})
