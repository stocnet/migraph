net <- as_tidygraph(data.frame(from = c("A", "B", "C", "D","E"),
                               to = c("B", "C", "D", "E", "A"))) %>%
  dplyr::mutate(name = c("Lisa", "John", "Lily", "Ben", "Adam"))

net2 <- as_tidygraph(data.frame(from = c("A", "B", "C", "D","E"),
                                to = c("B", "C", "D", "E", "A"))) %>%
  dplyr::mutate(friends = c("yes", "yes", "no", "no", "yes"))

name <- c("Lisa", "John", "Lily", "Ben", "Adam")

friends <- c("yes", "yes", "no", "no", "yes")

test_that("node_names works", {
  expect_equal(node_names(net), name)
})

test_that("node_attribute works", {
  expect_equal(node_attribute(net2, "friends"), friends)
})

test_that("graph_nodes works", {
  expect_equal(graph_nodes(net), 5)
})

test_that("graph_edges works", {
  expect_false(graph_edges(net)==10)
})
