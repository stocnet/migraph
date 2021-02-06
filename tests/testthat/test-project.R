mat1 <- matrix(c(0,1,1,0,0,1,1,1),2,4)
mat2 <- matrix(c(2,1,1,3),2,2)
mat3 <- matrix(c(1,0,1,1,0,1,0,1,1,0,1,1,1,1,1,2),4,4)

test_that("matrix projected correctly by rows",{
  expect_equal(project_rows(mat1), mat2)
  expect_true(igraph::is_weighted(project_rows(southern_women)))
  expect_true(tidygraph::is.tbl_graph(project_rows(mpn_elite_usa_advice)))
  expect_error(project_rows(as.data.frame(mat1)), "needs to be a tidygraph")
})

test_that("matrix projected correctly by columns",{
  expect_equal(project_cols(mat1), mat3)
  expect_true(igraph::is_weighted(project_cols(southern_women)))
  expect_true(tidygraph::is.tbl_graph(project_cols(mpn_elite_usa_advice)))
  expect_error(project_cols(as.data.frame(mat1)), "needs to be a tidygraph")
})
