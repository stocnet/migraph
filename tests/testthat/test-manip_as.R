# Tests for the as_ conversion methods

mat1 <- matrix(c(0,1,0,0,1,0,1,0,0,1,0,1,0,0,0,0),4,4, byrow = TRUE)
rownames(mat1) <- LETTERS[1:4]
colnames(mat1) <- LETTERS[1:4]
mat2 <- matrix(c(0,1,0,0,2,0,3,0,0,4,0,5,0,0,0,0),4,4, byrow = TRUE)
rownames(mat2) <- LETTERS[1:4]
colnames(mat2) <- LETTERS[1:4]
# Unweighted test
data1 <- dplyr::arrange(data.frame(from = c("A","B","B","C","C"),
                    to = c("B","C","A","D","B")),
                    from, to)
# Weighted test
data2 <- data1
data2$weight <- 1:5

# Data 3: misnamed weight col
data3 <- data1
data3$hello <- 1:5

test_that("data frame converted to matrix correctly",{
  expect_equal(as_matrix(data1), mat1)
  expect_equal(as_matrix(data2), mat2)
})

test_that("as_matrix converts correctly",{
  expect_vector(as_matrix(mat1))
  expect_vector(as_matrix(ison_southern_women))
  expect_vector(mpn_elite_mex %>% as_matrix())
  expect_vector(mpn_elite_usa_advice %>% as_matrix())
  expect_equal(as_matrix(as_network(ison_algebra)), as_matrix(ison_algebra))
})

test_that("as_igraph converts correctly",{
  expect_s3_class(as_igraph(mat1), "igraph")
  expect_s3_class(as_igraph(ison_southern_women), "igraph")
  expect_s3_class(as_igraph(as_network(ison_southern_women)), "igraph")
  expect_s3_class(as_igraph(mpn_elite_usa_money), "igraph")
  expect_error(as_igraph(data3, weight = T))
  expect_equal(network_nodes(as_igraph(as_network(data2))), network_nodes(as_igraph(data2)))
  # NB: ordering of edges is a little different when converting from network
  # to igraph. Should not matter though.
})

test_that("as_tidygraph converts correctly",{
  expect_s3_class(as_tidygraph(mat1), "tbl_graph")
  expect_s3_class(as_tidygraph(ison_southern_women), "tbl_graph")
  expect_s3_class(as_tidygraph(as_network(mat1)), "tbl_graph")
  expect_s3_class(as_tidygraph(as_network(ison_southern_women)), "tbl_graph")
  expect_s3_class(as_tidygraph(mpn_elite_usa_money), "tbl_graph")
})

test_that("as_network converts correctly",{
  expect_s3_class(as_network(mat1), "network")
  expect_s3_class(as_network(ison_southern_women), "network")
  expect_s3_class(as_network(mpn_elite_usa_money), "network")
  expect_equal(as_network(as_network(data2)), as_network(data2))
  expect_equal(as_network(as_igraph(ison_marvel_relationships)), as_network(ison_marvel_relationships))
  expect_equal(network_nodes(as_network(dplyr::as_tibble(data2))), network_nodes(as_network(data2)))
  expect_equal(is_directed(ison_southern_women), is_directed(as_network(ison_southern_women)))
  expect_equal(is_directed(mpn_DemSxP), is_directed(as_network(mpn_DemSxP)))
  expect_equal(is_directed(ison_adolescents), is_directed(as_network(ison_adolescents)))
  expect_equal(is_directed(ison_algebra), is_directed(as_network(ison_algebra)))
  # NB: ordering of edges is a little different when converting from network
  # to igraph. Should not matter though.
})

test_that("as_edgelist converts correctly", {
  expect_s3_class(as_edgelist(as_igraph(data2)), "tbl_df")
  expect_equal(as_edgelist(as_igraph(data2)), dplyr::as_tibble(data2))
  expect_equal(as_edgelist(as_igraph(data1)), dplyr::as_tibble(data1))
  expect_equal(as_edgelist(as_tidygraph(data2)), dplyr::as_tibble(data2))
  expect_equal(as_edgelist(as_tidygraph(data1)), dplyr::as_tibble(data1))
  expect_equal(as_edgelist(as_network(data1)), dplyr::as_tibble(data1))
  expect_equal(as_edgelist(as_network(data2)), dplyr::as_tibble(data2))
})

# test conversion of siena objects
# test_that("as_tidygraph.siena converts correctly", {
#   expect_equal(network_nodes(as_igraph(sienadata)), network_nodes(as_matrix(sienadata)))
#   expect_equal(network_nodes(as_igraph(sienadata)), length(sienadata[["nodeSets"]][["Actors"]]))
# })
