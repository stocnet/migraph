test_that("constraint scores are reported correctly for two-mode notworks",{
  expect_equal(round(unname(node_constraint(ison_southern_women)[1:3]),2), c(0.28, 0.31, 0.29))
  expect_named(node_constraint(ison_southern_women)[1:3], c("EVELYN", "LAURA", "THERESA"))
})

om <- igraph::graph(edges = c(1,2, 2,3), n = 4, directed = FALSE) 

test_that("constraint scores are reported correctly for one-mode notworks",{
  expect_equal(round(unname(node_constraint(mpn_elite_mex)[1:3]),2), c(0.45, 0.35, 0.28))
})

