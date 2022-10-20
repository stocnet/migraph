test_that("node_kernighanlin algorithm works", {
  expect_s3_class(node_kernighanlin(mpn_elite_mex), "node_member")
  expect_length(node_kernighanlin(mpn_elite_mex), 
                network_nodes(mpn_elite_mex))
  expect_false(any(node_kernighanlin(mpn_elite_mex) > 2))
})

test_that("node_edge_betweenness algorithm works", {
  expect_s3_class(node_edge_betweenness(mpn_elite_mex), "node_member")
  expect_length(node_edge_betweenness(mpn_elite_mex), 
                network_nodes(mpn_elite_mex))
})

test_that("node_fast_greedy algorithm works", {
  expect_s3_class(node_fast_greedy(ison_southern_women), "node_member")
  expect_length(node_fast_greedy(ison_southern_women), 
                network_nodes(ison_southern_women))
})

test_that("node_walktrap algorithm works", {
  expect_s3_class(node_walktrap(ison_southern_women), "node_member")
  expect_length(node_walktrap(ison_southern_women), 
                network_nodes(ison_southern_women))
})
