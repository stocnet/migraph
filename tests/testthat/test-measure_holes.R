test_that("redundancy is reported correctly", {
  expect_s3_class(node_redundancy(ison_brandes), "node_measure")
  expect_s3_class(node_redundancy(mpn_elite_usa_advice), "node_measure")
  expect_equal(length(node_redundancy(ison_brandes)), graph_nodes(ison_brandes))
  expect_equal(length(node_redundancy(mpn_elite_usa_advice)),
               graph_nodes(mpn_elite_usa_advice))
  expect_named(node_redundancy(mpn_elite_usa_advice))
})

test_that("effective size is calculated and reported correctly", {
  expect_s3_class(node_effsize(ison_brandes), "node_measure")
  expect_s3_class(node_effsize(mpn_elite_usa_advice), "node_measure")
  expect_equal(length(node_effsize(ison_brandes)), graph_nodes(ison_brandes))
  expect_equal(length(node_effsize(mpn_elite_usa_advice)),
               graph_nodes(mpn_elite_usa_advice))
  expect_named(node_effsize(mpn_elite_usa_advice))
  x <- node_degree(ison_southern_women, normalized = FALSE)[1] - node_redundancy(ison_southern_women)[1]
  expect_equal(node_effsize(ison_southern_women)[1], x)
})

test_that("efficiency is reported correctly", {
  expect_s3_class(node_efficiency(ison_brandes), "node_measure")
  expect_s3_class(node_efficiency(mpn_elite_usa_advice), "node_measure")
  expect_equal(length(node_efficiency(ison_brandes)), graph_nodes(ison_brandes))
  expect_equal(length(node_efficiency(mpn_elite_usa_advice)),
               graph_nodes(mpn_elite_usa_advice))
})

test_that("constraint scores are reported correctly for two-mode notworks",{
  expect_equal(round(unname(node_constraint(ison_southern_women)[1:3]),2), c(0.28, 0.31, 0.29))
  expect_named(node_constraint(ison_southern_women)[1:3], c("EVELYN", "LAURA", "THERESA"))
})

om <- igraph::graph(edges = c(1,2, 2,3), n = 4, directed = FALSE) 

test_that("constraint scores are reported correctly for one-mode notworks",{
  expect_equal(round(unname(node_constraint(mpn_elite_mex)[1:3]),2), c(0.45, 0.35, 0.28))
})

test_that("hierarchy is reported correctly", {
  expect_s3_class(node_hierarchy(ison_brandes), "node_measure")
  expect_s3_class(node_hierarchy(mpn_elite_usa_advice), "node_measure")
  expect_equal(length(node_hierarchy(ison_brandes)), graph_nodes(ison_brandes))
  expect_equal(length(node_hierarchy(mpn_elite_usa_advice)),
               graph_nodes(mpn_elite_usa_advice))
  expect_named(node_hierarchy(mpn_elite_usa_advice))
})
