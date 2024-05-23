# # Equivalence clustering tests

test_that("equivalence clustering returns the right class", {
  expect_s3_class(node_structural_equivalence(ison_adolescents, "strict", "hier"), "node_member")
  expect_s3_class(node_structural_equivalence(ison_adolescents, "elbow", "hier"), "node_member")
  expect_s3_class(node_structural_equivalence(ison_adolescents, "elbow", "concor"), "node_member")
  expect_s3_class(node_regular_equivalence(mpn_elite_mex), "node_member")
  expect_s3_class(node_automorphic_equivalence(mpn_elite_mex), "node_member")
})

test_that("equivalence clustering works", {
  expect_equal(node_structural_equivalence(ison_adolescents, "silhouette", "hier"), node_structural_equivalence(ison_adolescents))
  expect_equal(node_regular_equivalence(mpn_elite_mex), node_regular_equivalence(mpn_elite_mex, "silhouette", "hier"))
  expect_equal(network_nodes(ison_adolescents), length(node_structural_equivalence(ison_adolescents, "silhouette", "concor")))
  expect_equal(network_nodes(ison_adolescents), length(node_structural_equivalence(ison_adolescents, k = 3, "hier")))
  expect_equal(network_nodes(ison_adolescents), length(node_structural_equivalence(ison_adolescents, "strict", "concor")))
  expect_equal(network_nodes(mpn_elite_mex), length(node_regular_equivalence(mpn_elite_mex, cluster = "concor")))
  expect_equal(network_nodes(mpn_elite_mex), length(node_regular_equivalence(mpn_elite_mex, "elbow")))
  expect_equal(network_nodes(mpn_elite_mex), length(node_regular_equivalence(mpn_elite_mex, "strict")))
  expect_equal(network_nodes(mpn_elite_usa_advice), length(node_automorphic_equivalence(mpn_elite_usa_advice, "strict", distance = "binary")))
  expect_equal(network_nodes(mpn_elite_usa_advice), length(node_automorphic_equivalence(mpn_elite_usa_advice, distance = "maximum")))
  expect_true("C" %in% node_structural_equivalence(ison_adolescents, k = 3, "concor"))
  expect_true("B" %in% node_regular_equivalence(mpn_elite_mex, 2))
  expect_true("D" %in% node_automorphic_equivalence(mpn_elite_usa_advice, 4))
})

# from Traxler et al 2020
fig2 <- manynet::create_explicit(A--B, A--C, B--C, B--D, B--E, B--F, D--E)

test_that("to_correlation works", {
  expect_equal(to_correlation(fig2)["A","F"], 0.5773503, tolerance = 0.005)
  expect_equal(to_correlation(fig2, "diag")["A","F"], 0.5773503, tolerance = 0.005)
  expect_equal(to_correlation(fig2, "recip")["A","F"], 0.6123724, tolerance = 0.005)
  expect_equal(to_correlation(fig2, "all")["A","B"], -0.6324555, tolerance = 0.005)
  expect_equal(to_correlation(fig2, "complex")["A","B"], 0.3162278, tolerance = 0.005)
})
