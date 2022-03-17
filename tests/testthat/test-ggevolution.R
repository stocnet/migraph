test_that("ggevolution function works", {
  mpn_elite_mex2 <- mpn_elite_mex  %>%
    tidygraph::activate(edges) %>%
    tidygraph::reroute(from = sample.int(11, 44, replace = TRUE),
                       to = sample.int(11, 44, replace = TRUE))
  testplot <- ggevolution(mpn_elite_mex, mpn_elite_mex2)
  testplot2 <- ggevolution(mpn_elite_mex, mpn_elite_mex2, based_on = "last")
  testplot3 <- ggevolution(mpn_elite_mex, mpn_elite_mex2, based_on = "both")
  expect_true(is.list(testplot))
  expect_length(testplot, 2)
  expect_named(testplot[1], c('grobs', 'layout', 'widths', 'heights', 'respect', 'name', 'gp', 'vp', 'children', 'childrenOrder'))
  expect_false(isTRUE(all.equal(testplot, testplot2)))
  expect_false(isTRUE(all.equal(testplot, testplot3)))
  expect_error(ggevolution(mpn_elite_mex, mpn_elite_mex2, mpn_elite_mex2))
  expect_error(ggevolution(mpn_elite_mex, mpn_elite_mex2, based_on = "hello"))
})

# Note: Could be replaced by an internal time series network
test_that("ggatyear function works", {
  # Import test dataset
  # Note: The suppressed warnig relates to an igraph conversion method between
  # edgelists and tidygraph objects when the former contains NA's. This does
  # not affect the behavior of the function.
  test <- suppressWarnings(ggatyear(readRDS(test_path("sheets", "testtsnet.Rds")), 1990, node_color = "type", node_shape = "type", node_size = 0.3))
  expect_equal(class(test), c("ggraph", "gg", "ggplot"))
  expect_equal(names(test), c("data", "layers", "scales", "mapping",
                              "theme", "coordinates", 
                              "facet", "plot_env", "labels"))
  expect_true(all(test$data$circular == FALSE))
  expect_equal(class(test$data$type), "logical")
  expect_equal(length(test$data$type), 230)
  expect_equal(test[["labels"]][["title"]], 1990)
  expect_equal(unique(test[["layers"]][[2]][["aes_params"]][["shape"]]),
               c("circle", "square"))
})