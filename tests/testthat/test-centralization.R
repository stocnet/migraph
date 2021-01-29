test_tbl <- as_tidygraph(southern_women)
test_igr <- southern_women
test_mat <- as_matrix(southern_women)

test_that("two mode degree centralization calculated correctly", {
  expect_equal(round(centralisation_degree(test_igr, modes = "raw")$nodes1,4), 0.1813)
  expect_equal(round(centralisation_degree(test_igr, modes = "raw")$nodes2,4), 0.5097)
  expect_equal(round(centralisation_degree(test_igr, modes = "within")$nodes1,4), 0.2308)
  expect_equal(round(centralisation_degree(test_igr, modes = "within")$nodes2,4), 0.4661)
  expect_equal(round(centralisation_degree(test_igr, modes = "normalized")$nodes1,4), 0.2307)
  expect_equal(round(centralisation_degree(test_igr, modes = "normalized")$nodes2,4), 0.4661)
})

test_that("two mode closeness centralization calculated correctly", {
  expect_equal(round(centralisation_closeness(test_igr, modes = "normalized")$nodes1,4), 0.2843)
  expect_equal(round(centralisation_closeness(test_igr, modes = "normalized")$nodes2,4), 0.4418)
  expect_equal(round(centralisation_closeness(test_igr, modes = "within")$nodes1,4), 0.2135)
  expect_equal(round(centralisation_closeness(test_igr, modes = "within")$nodes2,4), 0.5285)
})

test_that("two mode betweenness centralization calculated correctly", {
  expect_equal(round(centralisation_betweenness(test_igr, modes = "raw")$nodes1,4), 0.0580)
  expect_equal(round(centralisation_betweenness(test_igr, modes = "raw")$nodes2,4), 0.2073)
  expect_equal(round(centralisation_betweenness(test_igr, modes = "within")$nodes1,4), 0.0668)
  expect_equal(round(centralisation_betweenness(test_igr, modes = "within")$nodes2,4), 0.1982)
  expect_equal(round(centralisation_betweenness(test_igr, modes = "normalized")$nodes1,4), 0.0586)
  expect_equal(round(centralisation_betweenness(test_igr, modes = "normalized")$nodes2,4), 0.2070)
})

test_that("one-mode centralisation is calculated correctly", {
  expect_equal(centralisation_degree(southern_women, modes = "one-mode"), 0.2641129)
  expect_equal(round(centralisation_closeness(southern_women, modes = "one-mode")), 0.319)
  expect_equal(round(centralisation_betweenness(southern_women, modes = "one-mode")),0.196)
})
