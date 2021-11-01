# Test layouts
set.seed(1000)
x <- layout_tbl_graph_frgrid(mpn_ryanair)$x
y <- layout_tbl_graph_frgrid(mpn_ryanair)$y
test_that("FR grid layout works",{
  expect_equal(head(x, 5), c(1, 0, 1, 1, 0))
  expect_equal(head(y, 5), c(-1, -1, -2, -1, -2))
  expect_length(layout_tbl_graph_frgrid(mpn_ryanair), 2)
  expect_true(class(attributes(layout_tbl_graph_frgrid(mpn_ryanair))$graph)[1]
              == "tbl_graph")
})

x <- layout_tbl_graph_kkgrid(mpn_ryanair)$x
y <- layout_tbl_graph_kkgrid(mpn_ryanair)$y
test_that("KK grid layout works",{
  expect_equal(head(x, 5), c(0, 1, -1, 1, 2))
  expect_equal(head(y, 5), c(1, 1, 1, 0 , 1))
  expect_length(layout_tbl_graph_frgrid(mpn_ryanair), 2)
  expect_true(class(attributes(layout_tbl_graph_frgrid(mpn_ryanair))$graph)[1]
              == "tbl_graph")
})

x <- layout_tbl_graph_gogrid(mpn_ryanair)$x
y <- layout_tbl_graph_gogrid(mpn_ryanair)$y
test_that("GO grid layout works",{
  # expect_equal(head(x, 5), c(-4, 5, -4, 4, -4))
  # expect_equal(head(y, 5), c(-4, 5, 5, -4, -4))
  expect_length(layout_tbl_graph_frgrid(mpn_ryanair), 2)
  expect_true(class(attributes(layout_tbl_graph_frgrid(mpn_ryanair))$graph)[1]
              == "tbl_graph")
})
