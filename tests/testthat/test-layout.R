# Test layouts
set.seed(1000)
x <- layout_tbl_graph_frgrid(mpn_ryanair)$x
y <- layout_tbl_graph_frgrid(mpn_ryanair)$y
test_that("FR grid layout works",{
  expect_true(all.equal(x, round(x)))
  expect_true(all.equal(y, round(y)))
  expect_length(layout_tbl_graph_frgrid(mpn_ryanair), 2)
  expect_true(class(attributes(layout_tbl_graph_frgrid(mpn_ryanair))$graph)[1]
              == "tbl_graph")
})

x <- layout_tbl_graph_kkgrid(mpn_ryanair)$x
y <- layout_tbl_graph_kkgrid(mpn_ryanair)$y
test_that("KK grid layout works",{
  expect_true(all.equal(x, round(x)))
  expect_true(all.equal(y, round(y)))
  expect_length(layout_tbl_graph_kkgrid(mpn_ryanair), 2)
  expect_true(class(attributes(layout_tbl_graph_kkgrid(mpn_ryanair))$graph)[1]
              == "tbl_graph")
})

x <- layout_tbl_graph_gogrid(mpn_ryanair)$x
y <- layout_tbl_graph_gogrid(mpn_ryanair)$y
test_that("GO grid layout works",{
  expect_true(all.equal(x, round(x)))
  expect_true(all.equal(y, round(y)))
  expect_length(layout_tbl_graph_gogrid(mpn_ryanair), 2)
  expect_true(class(attributes(layout_tbl_graph_gogrid(mpn_ryanair))$graph)[1]
              == "tbl_graph")
})
