# object without nodal attributes
net_node1 <- as_tidygraph(data.frame(
  from = c("A", "B", "C", "D","E"),
  to = c("B", "C", "D", "E", "A")))

# object with nodal attributes
net_node2 <- net_node1 %>%
  dplyr::mutate(attribute = c("friend", "family", "friend", "friend", "family"))

# object without edge attributes
net_edge1 <- data.frame(
  from = c("A", "B", "C", "D","E"),
  to = c("C", "D", "A", "A", "B"))

test_that("add_node_attributes works", {
  expect_equal(as_tidygraph(add_node_attributes(net_node1, "attribute", 
                                                c("friend", "family", "friend", "friend", "family"))), 
               net_node2)
})

test_that("copy_node_attributes works", {
  expect_equal(as_tidygraph(copy_node_attributes(net_node1, net_node2)), 
               net_node2)
})

test_that("add_edge_attributes works", {
  expect_equal(edge_attribute(add_edge_attributes(net_edge1, "weight", 
                                                  c(1,2,1,2,1)), 
                              "weight"), 
               c(1,2,1,2,1))
  expect_equal(class(add_edge_attributes(net_edge1, "weight", c(1,2,1,2,1))), 
               "igraph")
})
