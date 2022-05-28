test_that("node_kernighanlin algorithm works", {
  expect_equal(class(node_kernighanlin(mpn_elite_mex)), 
               c("partition", "numeric"))
  expect_length(node_kernighanlin(mpn_elite_mex), 
                graph_nodes(mpn_elite_mex))
  expect_false(any(node_kernighanlin(mpn_elite_mex) == 3))
})
