#' Helpers to grab various attributes from nodes or edges in a graph
#' @inheritParams as_igraph
#' @param attribute An attribute name.
#' @name grab
NULL

#' @rdname grab
#' @export
node_names <- function(object){
  igraph::get.vertex.attribute(as_igraph(object), "name")
}

#' @rdname grab
#' @export
node_attribute <- function(object, attribute){
  igraph::get.vertex.attribute(as_igraph(object), attribute)
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
  if(is_twomode(object)){
    out <- c(sum(!igraph::V(as_igraph(object))$type),
             sum(igraph::V(as_igraph(object))$type))
  } else {
    out <- igraph::vcount(as_igraph(object))
  }
  out
}

#' @rdname grab
#' @export
graph_edges <- function(object){
  igraph::ecount(as_igraph(object))
}

#' @rdname grab
#' @export
graph_dimensions <- function(object){
  if(is_twomode(object)){
    c(sum(!V(as_igraph(object))$type),
      sum(V(as_igraph(object))$type))
  } else {
    igraph::vcount(as_igraph(object))
  }
}

