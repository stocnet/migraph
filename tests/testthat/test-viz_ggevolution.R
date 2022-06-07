test_that("ggevolution function works", {
  mpn_elite_mex <- mpn_elite_mex %>% to_subgraph(in_mpn == 1)
  mpn_elite_mex2 <- mpn_elite_mex  %>%
    tidygraph::activate(edges) %>%
    tidygraph::reroute(from = sample.int(11, 44, replace = TRUE),
                       to = sample.int(11, 44, replace = TRUE))
  testplot <- ggevolution(mpn_elite_mex, mpn_elite_mex2)
  testplot2 <- ggevolution(mpn_elite_mex, mpn_elite_mex2, based_on = "last")
  testplot3 <- ggevolution(mpn_elite_mex, mpn_elite_mex2, based_on = "both")
  expect_true(is.list(testplot))
  expect_false(isTRUE(all.equal(testplot, testplot2)))
  expect_false(isTRUE(all.equal(testplot, testplot3)))
  expect_error(ggevolution(mpn_elite_mex, mpn_elite_mex2, mpn_elite_mex2))
  expect_error(ggevolution(mpn_elite_mex, mpn_elite_mex2, based_on = "hello"))
})
