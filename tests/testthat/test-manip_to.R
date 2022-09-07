test_that("to_unweight works", {
  women <- to_mode1(ison_southern_women)
  expect_true(is_weighted(women))
  expect_false(is_weighted(to_unweighted(women)))
})

test_that("to_unnamed works",{
  expect_true(is_labelled(ison_southern_women))
  expect_false(is_labelled(to_unnamed(ison_southern_women)))
})

test_that("to_undirected works",{
  expect_true(is_directed(ison_algebra))
  expect_false(is_directed(to_undirected(ison_algebra)))
})

test_that("to_redirected works",{
  expect_true(is_directed(to_redirected(mpn_ryanair)))
  expect_equal(colnames(as_matrix(mpn_ryanair)), 
               rownames(to_redirected(as_matrix(mpn_ryanair))))
})

test_that("to_onemode works",{
  expect_equal(c(to_onemode(ison_marvel_teams))[3], c(igraph::delete_vertex_attr(ison_marvel_teams, "type"))[3])
  expect_equal(as_matrix(to_onemode(as_tidygraph(ison_marvel_teams))),
               as_matrix(as_tidygraph(igraph::delete_vertex_attr(ison_marvel_teams, "type"))))
})

test_that("to_giant works",{
  expect_equal(graph_nodes(ison_marvel_relationships), 53)
  expect_equal(graph_nodes(to_giant(ison_marvel_relationships)), 50)
})

test_that("to_uniplex works", {
  expect_true(is_multiplex(ison_algebra))
  expect_false(is_multiplex(to_uniplex(ison_algebra, "friend_tie")))
})

test_that("to_simplex works", {
  expect_true(is_complex(ison_marvel_relationships))
  expect_false(is_complex(to_simplex(ison_marvel_relationships)))
})

test_that("to_unsigned works", {
  expect_true(is_signed(ison_marvel_relationships))
  expect_false(is_signed(to_unsigned(ison_marvel_relationships)))
  expect_false(all(as_matrix(to_unsigned(ison_marvel_relationships, "positive")) != 
                 as_matrix(to_unsigned(ison_marvel_relationships, "negative"))))
})

test_that("to_named works", {
  expect_false(is_labelled(ison_brandes))
  expect_true(is_labelled(to_named(ison_brandes)))
  expect_true(is_labelled(to_named(ison_brandes, seq_len(graph_nodes(ison_brandes)))))
})

test_that("multilevel works", {
  expect_true(is_twomode(mpn_elite_usa_advice))
  expect_false(is_twomode(to_multilevel(mpn_elite_usa_advice)))
})

test_that("matrix projected correctly by rows",{
  expect_true(is_weighted(to_mode1(ison_southern_women)))
  expect_true(all(node_names(to_mode1(ison_southern_women)) %in% node_names(ison_southern_women)))
  expect_true(length(node_names(to_mode1(ison_southern_women))) != length(node_names(ison_southern_women)))
  expect_equal(length(node_names(to_mode1(ison_southern_women))), length(rownames(as_matrix(ison_southern_women))))
  expect_equal(graph_nodes(to_mode1(ison_southern_women, "count")), graph_nodes(to_mode1(ison_southern_women, "jaccard")))
  expect_true(is_weighted(to_mode1(mpn_elite_usa_advice, "pearson")))
  expect_false(tie_weights(to_mode1(mpn_elite_usa_advice, "rand"))[3] == tie_weights(to_mode1(mpn_elite_usa_advice, "count"))[3])
})

test_that("matrix projected correctly by columns",{
  expect_true(is_weighted(to_mode2(ison_southern_women)))
  expect_true(all(node_names(to_mode2(ison_southern_women)) %in% node_names(ison_southern_women)))
  expect_true(length(node_names(to_mode2(ison_southern_women))) != length(node_names(ison_southern_women)))
  expect_equal(length(node_names(to_mode2(ison_southern_women))), length(colnames(as_matrix(ison_southern_women))))
  expect_equal(graph_nodes(to_mode2(ison_southern_women, "count")), graph_nodes(to_mode2(ison_southern_women, "jaccard")))
  expect_true(is_weighted(to_mode2(mpn_elite_usa_advice, "pearson")))
  expect_false(tie_weights(to_mode2(mpn_elite_usa_advice, "rand"))[1] == tie_weights(to_mode2(mpn_elite_usa_advice, "count"))[1])
})

test_that("to_blocks works", {
  block <- node_regular_equivalence(ison_algebra)
  block2 <- node_structural_equivalence(ison_southern_women)
  m1 <- block2[!node_mode(ison_southern_women)]
  m2 <- block2[node_mode(ison_southern_women)]
  expect_equal(max(block), ncol(to_blocks(as_matrix(ison_algebra), block)))
  expect_equal(ncol(to_blocks(as_matrix(ison_algebra), block)), nrow(to_blocks(as_matrix(ison_algebra), block)))
  expect_equal(ncol(as_matrix(to_blocks(ison_southern_women, block2))), max(m2))
  expect_equal(nrow(to_blocks(as_matrix(ison_southern_women), block2)), max(m1))
})

test_that("to matching works", {
  sw <- as_edgelist(to_matching(ison_southern_women))
  expect_equal(graph_nodes(to_matching(ison_southern_women)), graph_nodes(ison_southern_women))
  expect_true(nrow(sw) == nrow(dplyr::distinct(sw)))
})
