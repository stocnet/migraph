df <- data.frame( 
  person = c('Sam','Sam','Sam','Greg','Tom','Tom','Tom','Mary','Mary'), 
  group = c('a','b','c','b','b','c','d','b','d'), 
  stringsAsFactors = F)

df.mat <- as.matrix(df)

d <- igraph::graph(df.mat)

g <- igraph::as_edgelist(d)

df1 <- data.frame( 
  v1 = c('Sam', 'Greg', 'Tom' , 'Mary', 'a', 'b', 'c', 'd'), 
  v2 = c(.75, .25, .75, .25, 1, .5, .5),
  stringsAsFactors = F)

test_that("two mode degree centrality is correct",{
  expect_equal(twomode_centrality_degree(g), summary(df1))
})

# Need to test centralization for network and centrality for the list of nodes by 
# matching results
