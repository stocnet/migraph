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
node_mode <- function(object){
  if(is_twomode(object)) 
    igraph::get.vertex.attribute(as_igraph(object), "type")
  else rep(FALSE, graph_nodes(object))
}

#' @rdname grab
#' @export
node_attribute <- function(object, attribute){
  igraph::get.vertex.attribute(as_igraph(object), attribute)
}

#' @rdname grab
#' @export
edge_attribute <- function(object, attribute){
  igraph::get.edge.attribute(as_igraph(object), attribute)
}

#' @rdname grab
#' @export
edge_weights <- function(object){
  object <- as_igraph(object)
  out <- igraph::get.edge.attribute(object, "weight")
  if(is.null(out)) out <- rep(1, graph_edges(object))
  out
}

#' @describeIn grab Extracts the signs of the edges in a network.
#' @examples 
#' edge_signs(ison_marvel_relationships)
#' @export
edge_signs <- function(object){
  object <- as_igraph(object)
  out <- igraph::get.edge.attribute(object, "sign")
  if(is.null(out)) out <- rep(1, graph_edges(object))
  out
}

#' @describeIn grab Returns the number of nodes (of any mode) in a network.
#' @examples
#' graph_nodes(ison_southern_women)
#' @export
graph_nodes <- function(object){
  igraph::vcount(as_igraph(object))
}

#' @rdname grab
#' @export
graph_edges <- function(object){
  igraph::ecount(as_igraph(object))
}

#' @rdname grab
#' @export
graph_dims <- function(object){
  if(is_twomode(object)){
    c(sum(!igraph::V(as_igraph(object))$type),
      sum(igraph::V(as_igraph(object))$type))
  } else {
    igraph::vcount(as_igraph(object))
  }
}

#' @rdname grab
#' @importFrom igraph list.vertex.attributes
#' @export
graph_node_attributes <- function(object){
  igraph::list.vertex.attributes(as_igraph(object))
}

#' @rdname grab
#' @importFrom igraph list.edge.attributes
#' @export
graph_edge_attributes <- function(object){
  igraph::list.edge.attributes(as_igraph(object))
}