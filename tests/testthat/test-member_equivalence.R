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
  expect_true(3 %in% node_structural_equivalence(ison_adolescents, k = 3, "concor"))
  expect_true(2 %in% node_regular_equivalence(mpn_elite_mex, 2))
  expect_true(3 %in% node_automorphic_equivalence(mpn_elite_usa_advice, 4))
})
