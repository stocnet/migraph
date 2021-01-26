data1 <- data.frame(ID = LETTERS[1:3],
                    G = c(0,1,1),
                    H = c(0,0,1),
                    I = c(1,1,0))
data2 <- data.frame(id1 = c("A","B","B","C","C"),
                    id2 = c("I","G","I","G","H"))
mat1 <- matrix(c(0,1,1,0,0,1,1,1,0),3,3)
rownames(mat1) <- LETTERS[1:3]
colnames(mat1) <- LETTERS[7:9]

test_that("data frame converted to matrix correctly",{
  expect_equal(as_matrix(data1), mat1)
  expect_equal(as_matrix(data2), mat1)
})

test_that("as_igraph converts correctly",{
  expect_s3_class(as_igraph(mat1), "igraph")
  expect_s3_class(as_igraph(southern_women), "igraph")
  expect_s3_class(as_igraph(mpn_opensecrets), "igraph")
})

test_that("as_igraph converts correctly",{
  expect_vector(as_matrix(mat1))
  expect_vector(as_matrix(southern_women))
  expect_vector(as_matrix(mpn_opensecrets))
})
