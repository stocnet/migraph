net_node1 <- as_tidygraph(data.frame(
  from = c("A", "B", "C", "D","E"),
  to = c("B", "C", "D", "E", "A")))

net_node2 <- as_tidygraph(net_node1) %>%
  dplyr::mutate(attribute = c("friend", "family", "friend", "friend", "family"))

test_that("add_node_attributes works", {
  expect_equal(as_tidygraph(add_node_attributes(net_node1, "attribute", 
                                                c("friend", "family", "friend", "friend", "family"))), 
               net_node2)
})

test_that("copy_node_attributes works", {
  expect_equal(as_tidygraph(copy_node_attributes(net_node1, net_node2)), 
               net_node2)
})