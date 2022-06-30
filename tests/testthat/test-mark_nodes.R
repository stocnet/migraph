
test_that("node cuts works", {
  expect_s3_class(node_is_cutpoint(ison_algebra), "node_mark")
  expect_length(node_is_cutpoint(ison_southern_women),
                graph_nodes(ison_southern_women))
})
