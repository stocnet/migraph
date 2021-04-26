data1 <- data.frame(ID = LETTERS[1:3],
                    G = c(0,1,1),
                    H = c(0,0,1),
                    I = c(1,1,0))

mat1 <- matrix(c(0,1,1,0,0,1,1,1,0),3,3)
rownames(mat1) <- LETTERS[1:3]
colnames(mat1) <- LETTERS[7:9]

data2 <- data.frame(id1 = c("A","B","B","C","C"),
                    id2 = c("I","G","I","G","H"))

test_that("data frame converted to matrix correctly",{
  expect_equal(as_matrix(data2), mat1)
  expect_equal(as_matrix(data1), mat1) # fix issue with convert function that is not properly converting the data1 input
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

