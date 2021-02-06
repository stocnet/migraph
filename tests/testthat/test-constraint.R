test_that("constraint scores are reported correctly for two-mode notworks",{
  expect_equal(round(unname(node_constraint(southern_women)[[1]][1:3]),2), c(0.28, 0.31, 0.29))
  expect_equal(round(unname(node_constraint(southern_women)[[2]][1:3]),2), c(0.48, 0.48, 0.34))
})

om <- igraph::graph(edges = c(1,2, 2,3), n = 4, directed = FALSE) 

test_that("constraint scores are reported correctly for one-mode notworks",{
  expect_equal(round(unname(node_constraint(mpn_elite_mex)[1:3]),2), c(0.71, 0.39, 0.66))
})

