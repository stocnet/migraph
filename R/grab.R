#' Helpers to grab various attributes from nodes or edges in a graph
#' @inheritParams as_igraph
#' @name grab
NULL

#' @rdname grab
#' @export
node_names <- function(object){
  igraph::get.vertex.attribute(as_igraph(object), "name")
}

#' @rdname grab
#' @export
edge_weights <- function(object){
  object <- as_igraph(object)
  out <- igraph::get.edge.attribute(object, "weight")
  if(is.null(out)) out <- rep(1, graph_edges(object))
  out
}

#' @rdname grab
#' @export
graph_nodes <- function(object){
  igraph::vcount(as_igraph(object))
}

#' @rdname grab
#' @export
graph_edges <- function(object){
  igraph::ecount(as_igraph(object))
}