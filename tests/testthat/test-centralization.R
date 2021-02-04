test_that("two mode degree centralization calculated correctly", {
  expect_equal(round(centralisation_degree(southern_women, normalized = FALSE)$nodes1,4), 0.1813)
  expect_equal(round(centralisation_degree(southern_women, normalized = FALSE)$nodes2,4), 0.5097)
  expect_equal(round(centralisation_degree(southern_women, directed = "in")$nodes1,4), 0.2308)
  expect_equal(round(centralisation_degree(southern_women, directed = "in")$nodes2,4), 0.4661)
  expect_equal(round(centralisation_degree(southern_women, normalized = TRUE)$nodes1,4), 0.2268)
  expect_equal(round(centralisation_degree(southern_women, normalized = TRUE)$nodes2,4), 0.4744)
})

test_that("two mode closeness centralization calculated correctly", {
  expect_equal(round(centralisation_closeness(southern_women, normalized = TRUE)$nodes1,4), 0.2843)
  expect_equal(round(centralisation_closeness(southern_women, normalized = TRUE)$nodes2,4), 0.4418)
  expect_equal(round(centralisation_closeness(southern_women, directed = "in")$nodes1,4), 0.2135)
  expect_equal(round(centralisation_closeness(southern_women, directed = "in")$nodes2,4), 0.5285)
})

test_that("two mode betweenness centralization calculated correctly", {
  expect_equal(round(centralisation_betweenness(southern_women, normalized = FALSE)$nodes1,4), 0.0580)
  expect_equal(round(centralisation_betweenness(southern_women, normalized = FALSE)$nodes2,4), 0.2073)
  expect_equal(round(centralisation_betweenness(southern_women, directed = "in")$nodes1,4), 0.0668)
  expect_equal(round(centralisation_betweenness(southern_women, directed = "in")$nodes2,4), 0.1982)
  expect_equal(round(centralisation_betweenness(southern_women, normalized = TRUE)$nodes1,4), 0.0586)
  expect_equal(round(centralisation_betweenness(southern_women, normalized = TRUE)$nodes2,4), 0.2070)
})

test_that("one-mode centralisation is calculated correctly", {
  expect_equal(centralisation_degree(mpn_mexicanpower), 0.44)
  expect_equal(round(centralisation_closeness(mpn_mexicanpower), 3), 0.42)
  expect_equal(round(centralisation_betweenness(mpn_mexicanpower), 3), 0.218)
})
