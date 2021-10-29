test_weighted <- data.frame(
  source = c("A","A", "A", "A", "A","J", "B", "B", "C", "C", "D","I"),
  target = c("B","B", "C", "D", "J","A","E", "F", "G", "H", "I","I"),
  weight = sample(1:20, 12, replace = T)
)
test_unweighted <- data.frame(
  source = c("A","A", "A", "A", "A","J", "B", "B", "C", "C", "D","I"),
  target = c("B","B", "C", "D", "J","A","E", "F", "G", "H", "I","I")
)

simple_edge_df_weighted <- data.frame(
  from = c("b", "c", "c", "d", "a"),
  to = c("a", "b", "a", "a", "b"),
  weight = c(1, 1, 2, 2, 3)
)

simple_edge_df_unweighted <- data.frame(
  from = c("b", "c", "c", "d", "a"),
  to = c("a", "b", "a", "a", "b")
)

test_that("Unweight works", {
  expect_equal(to_unweighted(matrix(0:8,3,3)), matrix(c(0, rep(1,8)),3,3))
  expect_equal(to_unweighted(southern_women), southern_women)
  expect_equal(c(to_unnamed(as_matrix(to_unweighted(project_rows(southern_women)))))[1:4],
               c(0,1,1,1))
  expect_equal(c(to_unweighted(as_tidygraph(test_weighted)))[8],
               c(as_tidygraph(test_unweighted))[8])
  # expect_equal(c(to_unweighted(as_network(simple_edge_df_weighted))),
  #               c(as.matrix(as_network(simple_edge_df_unweighted))))
})

unnamedsouthern <- igraph::remove.vertex.attribute(southern_women, "name")
test_unnamed <- igraph::remove.vertex.attribute(as_tidygraph(test_weighted),
                                                "name")
test_that("Unnamed works",{
  expect_equal(to_unnamed(southern_women), igraph::remove.vertex.attribute(southern_women, "name"))
  expect_equal(c(to_unnamed(as_tidygraph(test_weighted)))[8], c(test_unnamed)[8])
  expect_equal(to_unnamed(as_network(southern_women)),
               network::delete.vertex.attribute(as_network(southern_women), "vertex.names"))
  expect_equal(c(as_matrix(to_unnamed(as_igraph(ison_m182)))),
               c(as_matrix(as_igraph(ison_m182))))
})

test_that("Undirected works",{
  expect_equal(c(to_undirected(as_igraph(ison_m182)))[3], c(igraph::as.undirected(ison_m182))[3])
  expect_equal(as_matrix(to_undirected(as_tidygraph(test_weighted))),
               as_matrix(as_tidygraph(igraph::as.undirected(as_tidygraph(test_weighted)))))
  expect_equal(to_undirected(as_matrix(ison_coleman)),
               ((as_matrix(ison_coleman) + t(as_matrix(ison_coleman))) > 0) * 1)
  expect_equal(to_undirected(as_matrix(southern_women)),
               as_matrix(southern_women))
  expect_equal(as_matrix(to_undirected(as_network(ison_coleman)))[1, ],
               as.numeric(as_matrix(as_network(sna::symmetrize(as_network(ison_coleman))))[1, ]))
})

test_that("To onemode works",{
  expect_equal(c(to_onemode(ison_marvel_teams))[3], c(igraph::delete_vertex_attr(ison_marvel_teams, "type"))[3])
  expect_equal(as_matrix(to_onemode(as_tidygraph(ison_marvel_teams))),
               as_matrix(as_tidygraph(igraph::delete_vertex_attr(ison_marvel_teams, "type"))))
})

comps <- igraph::components(ison_marvel_teams)
max.comp <- which.max(ison_marvel_teams$csize)
test <- igraph::delete.vertices(ison_marvel_teams, comps$membership != max.comp)

test_that("To main component works",{
  expect_equal(as_matrix(to_main_component(ison_marvel_teams)), as_matrix(test))
  expect_equal(as_matrix(to_main_component(as_tidygraph(ison_marvel_teams))),
               as_matrix(test))
})

out <- igraph::delete_edges(ison_m182,
                            igraph::E(ison_m182)[igraph::get.edge.attribute(ison_m182, "friend_tie") == 0])
edge_names <- igraph::edge_attr_names(ison_m182)
if (length(edge_names) > 1) {
  for (e in setdiff(edge_names, "friend_tie")) {
    out <- igraph::delete_edge_attr(out, e) 
  }
}
if (is.numeric(igraph::get.edge.attribute(ison_m182, "friend_tie"))) names(igraph::edge_attr(out)) <- "weight"

test_that("To Uniplex works", {
  expect_equal(as_matrix(to_uniplex(ison_m182, "friend_tie")), as_matrix(out))
  expect_equal(as_matrix(to_uniplex(as_tidygraph(ison_m182), "friend_tie")), as_matrix(out))
})

test_that("To simplex works", {
  expect_equal(as_matrix(to_simplex(ison_m182)), as_matrix(ison_m182))
})

testunsigned <- igraph::delete_edges(ison_marvel_relationships, which(igraph::E(ison_marvel_relationships)$sign < 0))
testunsigned2 <- igraph::delete_edges(ison_m182, which(igraph::E(ison_m182)$sign < 0))

test_that("unsigned works", {
  expect_equal(c(as_matrix(to_unsigned(ison_marvel_relationships, "positive"))),
               c(as_matrix(testunsigned)))
  expect_equal(c(as_matrix(to_unsigned(ison_m182, "positive"))),
               c(as_matrix(testunsigned2)))
})

testnamed <- ison_m182
igraph::V(testnamed)$name  <- sample(baby_names,
                                     igraph::vcount(testnamed))
test_that("to_named works", {
  expect_equal(c(as_matrix(to_named(ison_m182))), c(as_matrix(testnamed)))
  expect_equal(c(as_matrix(to_named(as_igraph(ison_m182)))), c(as_matrix(testnamed)))
})

testmultilevel <- mpn_elite_usa_advice
igraph::V(testmultilevel)$lvl <- ifelse(igraph::V(testmultilevel)$type, 2, 1)
testmultilevel <- igraph::delete_vertex_attr(testmultilevel, "type")
test_that("multilevel works", {
  expect_equal(to_multilevel(mpn_elite_usa_advice), testmultilevel)
  expect_equal(to_multilevel(as_igraph(mpn_elite_usa_advice)), as_igraph(testmultilevel))
})
