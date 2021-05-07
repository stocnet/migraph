test_that("unweight works", {
  expect_equal(to_unweighted(matrix(0:8,3,3)), matrix(c(0, rep(1,8)),3,3))
  expect_equal(to_unweighted(southern_women), southern_women)
  expect_equal(c(to_unnamed(as_matrix(to_unweighted(project_rows(southern_women)))))[1:4], c(0,1,1,1))
})
