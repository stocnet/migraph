test_that("node_kernighanlin algorithm works", {
  expect_s3_class(node_kernighanlin(mpn_elite_mex), "node_member")
  expect_length(node_kernighanlin(mpn_elite_mex), 
                graph_nodes(mpn_elite_mex))
  expect_false(any(node_kernighanlin(mpn_elite_mex) > 2))
})
