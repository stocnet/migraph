#' Tools for reformatting networks, graphs, and matrices
#' 
#' Note that `to_onemode()`, which is currently only implemented for igraph,
#' is not the same as `project_rows()` and `project_cols()`.
#' There is no transformation involved; `to_onemode()` simply deletes the 'type'
#' attribute from vertices, removing the bipartite note, but retaining all
#' vertices.
#' @name to
#' @param object A matrix, `{igraph}` graph, `{tidygraph}` tbl_graph, or
#' `{network}` object.
#' @param edge the name of an edge attribute to retain from a graph
#' @param keep in the case of a signed network, whether to retain
#' the "positive" or "negative" ties
#' @param threshold For a matrix, the threshold to binarise/dichotomise at.
#' @examples
#' to_unweighted(project_rows(southern_women))
#' @export
to_unweighted <- function(object, threshold = 1) UseMethod("to_unweighted")

#' @export
to_unweighted.tbl_graph <- function(object, threshold = 1) {
    out <- igraph::delete_edge_attr(object, "weight")
    tidygraph::as_tbl_graph(out)
}

#' @export
to_unweighted.igraph <- function(object, threshold = 1) {
    if ("weight" %in% igraph::edge_attr_names(object)) {
      igraph::delete_edge_attr(object, "weight")
    } else object
}

#' @export
to_unweighted.network <- function(object, threshold = 1) {
    out <- as_igraph(object)
    out <- igraph::delete_edge_attr(out, "weight")
    as_network(out)
}

#' @export
to_unweighted.matrix <- function(object, threshold = 1) {
  object <- (object >= threshold)*1
  object
}

#' @rdname to
#' @examples
#' to_unnamed(project_rows(southern_women))
#' @export
to_unnamed <- function(object) UseMethod("to_unnamed")

#' @export
to_unnamed.igraph <- function(object) {
  if ("name" %in% igraph::vertex_attr_names(object)) {
    igraph::delete_vertex_attr(object, "name")
  } else object
}

#' @export
to_unnamed.tbl_graph <- function(object) {
    out <- igraph::delete_vertex_attr(object, "name")
    tidygraph::as_tbl_graph(out)
}

#' @export
to_unnamed.network <- function(object) {
  out <- as_igraph(object)
  out <- igraph::delete_vertex_attr(out, "name")
  as_network(out)
}

#' @export
to_unnamed.matrix <- function(object) {
  out <- object
  rownames(out) <- NULL
  colnames(out) <- NULL
  out
}

#' @rdname to
#' @examples
#' to_undirected(ison_coleman)
#' @export
to_undirected <- function(object) UseMethod("to_undirected")

#' @importFrom igraph as.undirected
#' @export
to_undirected.igraph <- function(object) {
  igraph::as.undirected(object)
}

#' @importFrom igraph as.undirected
#' @export
to_undirected.tbl_graph <- function(object) {
  as_tidygraph(igraph::as.undirected(object))
}

#' @importFrom sna symmetrize
#' @export
to_undirected.network <- function(object) {
  sna::symmetrize(object)
}

#' @export
to_undirected.matrix <- function(object) {
  if (is_twomode(object)) {
    object
  } else ((object + t(object)) > 0) * 1
}

#' @rdname to
#' @importFrom igraph delete_vertex_attr
#' @examples
#' to_onemode(ison_marvel_teams)
#' @export
to_onemode <- function(object) UseMethod("to_onemode")

#' @export
to_onemode.tbl_graph <- function(object) {
  as_tidygraph(to_onemode(as_igraph(object)))
}

#' @export
to_onemode.igraph <- function(object) {
  object <- igraph::delete_vertex_attr(object, "type")
  object
}

#' @rdname to
#' @export
to_main_component <- function(object) UseMethod("to_main_component")

#' @export
to_main_component.tbl_graph <- function(object) {
  as_tidygraph(to_main_component(as_igraph(object)))
}

#' @export
to_main_component.igraph <- function(object) {
  comps <- igraph::components(object)
  max.comp <- which.max(comps$csize)
  igraph::delete.vertices(object, comps$membership != max.comp)
}

#' @rdname to
#' @importFrom igraph delete_edges edge_attr_names delete_edge_attr
#' @importFrom igraph E get.edge.attribute edge_attr_names
#' @examples
#' to_uniplex(ison_m182, "friend_tie")
#' @export
to_uniplex <- function(object, edge) UseMethod("to_uniplex")

#' @export
to_uniplex.tbl_graph <- function(object, edge){
  as_tidygraph(to_uniplex(as_igraph(object), edge))
}

#' @export
to_uniplex.igraph <- function(object, edge){
  out <- igraph::delete_edges(object,
                              igraph::E(object)[igraph::get.edge.attribute(object, edge) == 0])
  edge_names <- igraph::edge_attr_names(object)
  if (length(edge_names) > 1) {
    for (e in setdiff(edge_names, edge)) {
      out <- igraph::delete_edge_attr(out, e) 
    }
  }
  if(is.numeric(igraph::get.edge.attribute(object, edge))) names(igraph::edge_attr(out)) <- "weight"
  out
}

#' @rdname to
#' @examples
#' to_unsigned(ison_marvel_relationships, "positive")
#' to_unsigned(ison_marvel_relationships, "negative")
#' @export
to_unsigned <- function(object, keep = c("positive", "negative")) UseMethod("to_unsigned")

#' @export
to_unsigned.tbl_graph <- function(object, keep = c("positive", "negative")){
  out <- to_unsigned(as_igraph(object))
  as_tidygraph(out)
}

#' @export
to_unsigned.igraph <- function(object, keep = c("positive", "negative")){
  if(is_signed(object)){
    keep <- match.arg(keep)
    if(keep == "positive"){
      out <- igraph::delete_edges(object, which(igraph::E(object)$sign < 0))
    } else {
      out <- igraph::delete_edges(object, which(igraph::E(object)$sign > 0))
    }
    out <- igraph::delete_edge_attr(out, "sign")
    out
  } else object
}

#' @rdname to
#' @importFrom igraph simplify
#' @examples
#' to_simplex(ison_m182)
#' @export
to_simplex <- function(object) UseMethod("to_simplex")

#' @export
to_simplex.tbl_graph <- function(object) {
  as_tidygraph(to_simplex(as_igraph(object)))
}

#' @export
to_simplex.igraph <- function(object) {
  igraph::simplify(object)
}

#' @rdname to
#' @examples
#' to_named(ison_m182)
#' @export
to_named <- function(object) UseMethod("to_named")

#' @export
to_named.tbl_graph <- function(object) {
  object %>% mutate(name = sample(baby_names,
                                  igraph::vcount(object)))
}

#' @export
to_named.igraph <- function(object) {
  igraph::V(object)$name  <- sample(baby_names,
                                    igraph::vcount(object))
  object
}

#' @rdname to
#' @examples
#' to_multilevel(mpn_elite_usa_advice)
#' @export
to_multilevel <- function(object) UseMethod("to_multilevel")

#' @export
to_multilevel.tbl_graph <- function(object) {
  as_tidygraph(to_multilevel(as_igraph(object)))
}

#' @export
to_multilevel.igraph <- function(object) {
  igraph::V(object)$lvl <- ifelse(igraph::V(object)$type, 2, 1)
  object <- igraph::delete_vertex_attr(object, "type")
  object
}

