# Clustering tests

x <- node_structural_equivalence(ison_adolescents, "hier", "strict")
test_that("structural equivalence clustering works", {
  expect_equal(class(node_structural_equivalence(mpn_elite_mex)),
               c("partition", "integer"))
  expect_false(names(x[1]) %in% names(x[2:length(x)]))
})

test_that("regular equivalence clustering works", {
  expect_equal(class(node_regular_equivalence(mpn_elite_mex)),
               c("partition", "integer"))
})

y <- node_automorphic_equivalence(mpn_elite_mex, "elbow")
test_that("automorphic equivalence clustering works", {
  expect_equal(class(y), c("partition", "integer"))
  expect_false(names(y[1]) %in% names(y[2:length(y)]))
})
