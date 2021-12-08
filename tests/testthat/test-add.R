net_node1 <- as_tidygraph(data.frame(
  from = c("A", "B", "C", "D","E"),
  to = c("B", "C", "D", "E", "A")))

net_node2 <- net_node1 %>%
  dplyr::mutate(attribute = c("friend", "family", "friend", "friend", "family"))

# net_edge1 <- as_tidygraph(data.frame(
#   from = c("A", "B", "C", "D","E"),
#   to = c("C", "D", "E", "A", "B")))
# 
# net_edge2 <- as_tidygraph(data.frame(
#   from = c("A", "B", "C", "D","E"),
#   to = c("D", "D", "E", "A", "C")))

# net_edge3 <- as_tidygraph(data.frame(
#   from = c("A", "A", "B", "C", "D","E", "E"),
#   to = c("C", "D", "D", "E", "A", "B", "C"))) %>%
#   igraph::set_edge_attr("attribute", value = c(0, 1, 1, 1, 1, 0, 1))
# 
# net_edge4 <- net_edge1 %>%
#   igraph::set_edge_attr("weight", value = c(0, 1, 1, 1, 1))

test_that("add_node_attributes works", {
  expect_equal(as_tidygraph(add_node_attributes(net_node1, "attribute", 
                                                c("friend", "family", "friend", "friend", "family"))), 
               net_node2)
})

test_that("copy_node_attributes works", {
  expect_equal(as_tidygraph(copy_node_attributes(net_node1, net_node2)), 
               net_node2)
})

# test_that("add_edge_attributes works", {
#   expect_equal(add_edge_attributes(net_edge1, net_edge4), net_edge4)
# })

# test_that("mutate_edges works", {
#   expect_equal(mutate_edges(net_edge1, net_edge2, "attribute"), net_edge3)
# })
