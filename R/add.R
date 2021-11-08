#' Adding and copying attributes from one graph to another
#' 
#' @inheritParams as_igraph
#' @param object2 A second object to copy nodes or edges from.
#' @param attr_name Name of the new attribute in the resulting object.
#' @param vector A vector of values for the new attribute.
#' @name add
NULL

#' @rdname add
#' @importFrom igraph vertex_attr<-
#' @export
add_node_attributes <- function(object, attr_name, vector){
  if(length(vector)!=graph_nodes(object)) 
    stop("Attribute vector must be same length as nodes in object.")
  object <- as_igraph(object)
  vertex_attr(object, name = attr_name) <- vector
  object
}

#' @rdname add
#' @export
add_edge_attributes <- function(object, object2){
  object <- tidygraph::graph_join(as_tidygraph(object), as_tidygraph(object2))
  object
}

#' @rdname add
#' @export
copy_node_attributes <- function(object, object2){
  if(graph_nodes(object) != graph_nodes(object2)) stop("Objects need to be of compatible dimensions.")
  object <- as_igraph(object)
  object2 <- as_igraph(object2)
  for(a in igraph::vertex_attr_names(object2)){
    object <- igraph::set.vertex.attribute(object, a, value = igraph::get.vertex.attribute(object2, a))
  }
  object
}