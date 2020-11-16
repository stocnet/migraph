mat1 <- matrix(c(0,1,1,0,0,1,1,1,0,0,1,0,1,0,1,0),4,4)
rownames(mat1) <- c("Sam", "Mary", "John", "Ana")
colnames(mat1) <- LETTERS[7:10]

mat2 <- matrix(c(Sam = .25, Mary = .5, John = 1, Ana = .25))

test_that("two mode degree centrality is correct",{
  expect_equal(twomode_centrality_degree(mat1), mat2)
})

# Need to test centralization for network and centrality for the list of nodes by 
# matching results