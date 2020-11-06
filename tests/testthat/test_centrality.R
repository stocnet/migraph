df <- data.frame( 
  person = c('Sam','Sam','Sam','Greg','Tom','Tom','Tom','Mary','Mary'), 
  group = c('a','b','c','a','b','c','d','b','d'), 
  stringsAsFactors = F)

df.mat <- as.matrix(df)

g <- graph.edgelist(df.mat, directed = FALSE)

test_that("two mode degree centrality is correct",{
  expect_equal(...)
})

# Need to test centralization for network and centrality for the list of nodes by 
# matching results

