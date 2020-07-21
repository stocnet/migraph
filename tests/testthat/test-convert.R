data1 <- data.frame(ID = LETTERS[1:3],
                    G = c(0,1,1),
                    H = c(0,0,1),
                    I = c(1,1,0))
mat1 <- matrix(c(0,1,1,0,0,1,1,1,0),3,3)
rownames(mat1) <- LETTERS[1:3]
colnames(mat1) <- LETTERS[7:9]

test_that("data frame converted to matrix correctly",{
  expect_equal(df_to_mat(data1), mat1)
})