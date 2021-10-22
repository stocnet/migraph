test_that("graph_balance calculates balance score correctly", {
  expect_equal(round(graph_balance(ison_marvel_relationships, "triangles"),4), 0.6683)
})
