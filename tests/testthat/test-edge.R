net <- data.frame(
  from = c("A", "B", "C", "D","F"),
  to = c("B", "A", "D", "F", "C"))

test_that("edge_mutual works", {
  expect_length(edge_mutual(net), 5)
})
