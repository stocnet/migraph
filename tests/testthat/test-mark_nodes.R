
test_that("node cuts works", {
  expect_s3_class(node_is_cutpoint(ison_algebra), "node_mark")
  expect_length(node_is_cutpoint(ison_southern_women),
                network_nodes(ison_southern_women))
})

test_that("node isolate works", {
  expect_s3_class(node_is_isolate(ison_brandes), "logical")
  expect_equal(length(node_is_isolate(ison_brandes)), network_nodes(ison_brandes))
})

test_that("node_is_max works", {
  expect_equal(length(node_is_max(node_betweenness(ison_brandes))), network_nodes(ison_brandes))
  expect_equal(sum(node_is_max(node_betweenness(ison_brandes)) == TRUE), 1)
  expect_s3_class(node_is_max(node_betweenness(ison_brandes)), "logical")
})

test_that("node_is_min works", {
  expect_equal(length(node_is_min(node_betweenness(ison_brandes))), network_nodes(ison_brandes))
  expect_equal(sum(node_is_min(node_betweenness(ison_brandes)) == TRUE), 4)
  expect_s3_class(node_is_min(node_betweenness(ison_brandes)), "logical")
})
