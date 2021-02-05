om <- igraph::graph( edges=c(1,2, 2,3, 3,1, 4,5, 5,6), n=9, directed=F) 

test_that("one-mode object clustering is reported correctly",{
  expect_equal(round(graph_clustering(om), 2), 0.75)
})

test_that("two-mode object clustering is reported correctly",{
  expect_equal(round(graph_clustering(southern_women), 4), 0.4872)
})

mat1 <- create_ring(5,10)
mat2 <- create_ring(5,8)
 
test_that("three-mode clustering calculated correctly",{
expect_equal(round(graph_clustering(mat1, mat2), 4), 0.7143)
})
