# Equivalence tests

x <- node_structural_equivalence(ison_adolescents, "strict", "hier")
test_that("structural equivalence clustering works", {
  expect_s3_class(x, "member")
  expect_false(names(x[1]) %in% names(x[2:length(x)]))
})

test_that("regular equivalence clustering works", {
  expect_s3_class(node_regular_equivalence(mpn_elite_mex), "member")
})

y <- node_automorphic_equivalence(mpn_elite_mex, "elbow")
test_that("automorphic equivalence clustering works", {
  expect_s3_class(y, "member")
  expect_false(names(y[1]) %in% names(y[2:length(y)]))
})
