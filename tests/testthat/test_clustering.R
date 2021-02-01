om <- igraph::graph( edges=c(1,2, 2,3, 3,1, 4,5, 5,6), n=9, directed=F) 

test_that("one-mode object clustering is reported correctly",{
  expect_equal(round(clustering(om), 2), 0.75)
})

test_that("two-mode object clustering is reported correctly",{
  expect_equal(round(clustering(southern_women), 4), 0.4872)
})

mat1 <- matrix(0,3,3)
mat1[1,1:2] <- 1
mat1[2,2] <- 1
mat2 <- matrix(0,3,3)
mat2[1:2,1] <- 1
mat2[3,3] <- 1
 
# test_that("three-mode clustering calculated correctly",{
#  expect_equal(clustering(mat1, mat2), 2/3)
# })
