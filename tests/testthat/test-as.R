mat1 <- matrix(c(0,1,1,0,0,1,1,1,0),3,3)
rownames(mat1) <- LETTERS[1:3]
colnames(mat1) <- LETTERS[7:9]

data1 <- data.frame(from = c("A","B","B","C","C"),
                    to = c("I","G","I","G","H"))
data2 <- data1
data2$weight <- 1:5

test_that("data frame converted to matrix correctly",{
  a <- as_matrix(data1)
  expect_true(is.matrix(a))
  expect_equal(as_matrix(data2), mat1)
})

test_that("as_matrix converts correctly",{
  expect_vector(as_matrix(mat1))
  expect_vector(as_matrix(southern_women))
  expect_vector(mpn_elite_mex %>% as_matrix())
  expect_vector(mpn_elite_usa_advice %>% as_matrix())
  expect_equal(as_matrix(as_network(southern_women)), as_matrix(southern_women))
})

test_that("as_igraph converts correctly",{
  expect_s3_class(as_igraph(mat1), "igraph")
  expect_s3_class(as_igraph(southern_women), "igraph")
  expect_s3_class(as_igraph(as_network(southern_women)), "igraph")
  expect_s3_class(as_igraph(mpn_elite_usa_money), "igraph")
})

test_that("as_tidygraph converts correctly",{
  expect_s3_class(as_tidygraph(mat1), "tbl_graph")
  expect_s3_class(as_tidygraph(southern_women), "tbl_graph")
  expect_s3_class(as_tidygraph(as_network(mat1)), "tbl_graph")
  expect_s3_class(as_tidygraph(as_network(southern_women)), "tbl_graph")
  expect_s3_class(as_tidygraph(mpn_elite_usa_money), "tbl_graph")
})

test_that("as_network converts correctly",{
  expect_s3_class(as_network(mat1), "network")
  expect_s3_class(as_network(southern_women), "network")
  expect_s3_class(as_network(mpn_elite_usa_money), "network")
})

data3 <- igraph::graph_from_edgelist(as.matrix(data2))

test_that("as_edgelist converts correctly", {
  expect_s3_class(as_edgelist(as_igraph(data2)), "tbl_df")
  expect_equal(as_edgelist(as_igraph(data2)), tibble::as_tibble(data2))
  expect_equal(as_edgelist(as_igraph(data1)), tibble::as_tibble(data1))
  expect_equal(as_edgelist(as_tidygraph(data2)), tibble::as_tibble(data2))
  expect_equal(as_edgelist(as_tidygraph(data1)), tibble::as_tibble(data1))
})
