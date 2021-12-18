net <- as_tidygraph(data.frame(from = c("A", "B", "C", "D","E"),
                               to = c("B", "C", "D", "E", "A"))) %>%
  dplyr::mutate(name = c("Lisa", "John", "Lily", "Ben", "Adam")) %>%
  dplyr::mutate(gender = c("female", "male", "female", "male", "male"))

net2 <- as_tidygraph(data.frame(from = c("A", "B", "C", "D","E"),
                                to = c("B", "C", "D", "E", "A"))) %>%
  dplyr::mutate(friends = c("yes", "yes", "no", "no", "yes")) %>%
  igraph::set_edge_attr("weight", value = 1:5)

net3 <- as_matrix(data.frame(from = c("A", "A", "B", "C", "D", "D", "E", "E"),
                   to = c("B", "G", "C", "D", "E", "G", "A", "H")))

name <- c("Lisa", "John", "Lily", "Ben", "Adam")

friends <- c("yes", "yes", "no", "no", "yes")

test_that("node_names works", {
  expect_equal(node_names(net), name)
  expect_length(node_names(net), 5)
})

test_that("node_attribute works", {
  expect_equal(node_attribute(net2, "friends"), friends)
  expect_length(node_attribute(net2, "friends"), igraph::vcount(net2))
})

test_that("edge_attribute works", {
  expect_equal(edge_attribute(net2, "weight"), c(1, 2, 3, 4, 5))
})

test_that("edge_weights works", {
  expect_equal(edge_weights(net2), edge_attribute(net2, "weight"))
})

test_that("graph_nodes works", {
  expect_equal(graph_nodes(net), 5)
})

test_that("graph_edges works", {
  expect_false(graph_edges(net)==10)
})

test_that("graph_dims works", {
  expect_equal(graph_dims(net3), c(5, 7))
  expect_length(graph_dims(net3), 2)
  expect_false(length(graph_dims(net3)) == 1)
})

test_that("graph_node_attributes works", {
  expect_equal(graph_node_attributes(net), c("name", "gender"))
  expect_length(graph_node_attributes(net), 2)
})

test_that("graph_edge_attributes works", {
  expect_equal(graph_edge_attributes(net2), "weight")
  expect_length(graph_edge_attributes(net2), 1)
})
