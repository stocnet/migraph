names <- c("Lisa", "John", "Lily", "Ben", "Adam")
net <- as_tidygraph(data.frame(from = c("A", "B", "C", "D","E"),
                               to = c("B", "C", "D", "E", "A"))) %>%
  mutate(name = names) %>%
  mutate(gender = c("female", "male", "female", "male", "male"))

net2 <- as_tidygraph(data.frame(from = c("A", "B", "C", "D","E"),
                                to = c("B", "C", "D", "E", "A"))) %>%
  mutate(friends = c("yes", "yes", "no", "no", "yes")) %>%
  igraph::set_edge_attr("weight", value = 1:5)

net3 <- as_matrix(data.frame(from = c("A", "A", "B", "C", "D", "D", "E", "E"),
                   to = c("B", "G", "C", "D", "E", "G", "A", "H")))

friends <- c("yes", "yes", "no", "no", "yes")

test_that("node_names works", {
  expect_equal(node_names(net), names)
  expect_length(node_names(net), 5)
})

test_that("node_attribute works", {
  expect_equal(node_attribute(net2, "friends"), friends)
  expect_length(node_attribute(net2, "friends"), igraph::vcount(net2))
})

test_that("node_mode works", {
  expect_equal(as.logical(node_mode(ison_southern_women)[1]),
               as.logical(!node_mode(ison_southern_women)[length(ison_southern_women)]))
  expect_s3_class(node_mode(ison_southern_women), c("node mark", "logical"))
})

test_that("tie_attribute works", {
  expect_equal(tie_attribute(net2, "weight"), c(1, 2, 3, 4, 5))
})

test_that("tie_weights works", {
  expect_equal(tie_weights(net2), tie_attribute(net2, "weight"))
})

test_that("network_nodes works", {
  expect_equal(network_nodes(net), 5)
})

test_that("network_ties works", {
  expect_equal(network_ties(net), 5)
})

test_that("network_dims works", {
  expect_equal(network_dims(ison_karateka), 34)
  expect_equal(network_dims(ison_southern_women), c(18,14))
})

test_that("network_node_attributes works", {
  expect_equal(network_node_attributes(net), c("name", "gender"))
  expect_length(network_node_attributes(net), 2)
})

test_that("network_tie_attributes works", {
  expect_equal(network_tie_attributes(net2), "weight")
  expect_length(network_tie_attributes(net2), 1)
})
