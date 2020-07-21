data1 <- data.frame(ID = LETTERS[1:3],
                    G = c(0,1,1),
                    H = c(0,0,1),
                    I = c(1,1,0))
mat1 <- matrix(c(0,1,1,0,0,1,1,1,0),3,3)
rownames(mat1) <- LETTERS[1:3]
colnames(mat1) <- LETTERS[7:9]

mat4 <- matrix(c(0,1,1,0,0,1,1,1),4,2)
mat2 <- matrix(c(0,0,0,0,0,2,2,1,0,2,2,1,0,1,1,1),4,4)
mat3 <- matrix(c(2,2,2,3),2,2)

test_that("data frame converted to matrix correctly",{
  expect_equal(df_to_mat(data1), mat1)
})

test_that("two-mode matrix projected correctly",{
  expect_equal(row_project(mat4), mat2)
  expect_equal(col_project(mat4), mat3)
})
