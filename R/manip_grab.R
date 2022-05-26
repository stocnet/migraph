#' Helpers to grab various attributes from nodes or edges in a graph
#' @inheritParams is
#' @param attribute Character string naming an attribute in the object.
#' @name grab
NULL

#' @describeIn grab Extracts the names of the nodes in a network.
#' @examples 
#' node_names(mpn_elite_usa_advice)
#' @export
node_names <- function(object){
  igraph::get.vertex.attribute(as_igraph(object), "name")
}

#' @describeIn grab Extracts the mode of the nodes in a network.
#' @examples 
#' node_mode(mpn_elite_usa_advice)
#' @export
node_mode <- function(object){
  if(is_twomode(object)) 
    igraph::get.vertex.attribute(as_igraph(object), "type")
  else rep(FALSE, graph_nodes(object))
}

#' @describeIn grab Extracts an attribute's values for the nodes in a network.
#' @examples
#' node_attribute(mpn_elite_mex, "full_name")
#' @export
node_attribute <- function(object, attribute){
  igraph::get.vertex.attribute(as_igraph(object), attribute)
}

#' @describeIn grab Extracts an attribute's values for the edges in a network.
#' @examples
#' edge_attribute(ison_algebra, "task_tie")
#' @export
edge_attribute <- function(object, attribute){
  igraph::get.edge.attribute(as_igraph(object), attribute)
}

#' @describeIn grab Extracts the weights of the edges in a network.
#' @examples
#' edge_weights(to_mode1(ison_southern_women))
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

#' @describeIn grab Returns the number of edges in a network.
#' @examples
#' graph_edges(ison_southern_women)
#' @export
graph_edges <- function(object){
  igraph::ecount(as_igraph(object))
}

#' @describeIn grab Returns the dimensions of a network in a vector
#'   as long as the number of modes in the network.
#' @examples
#' graph_dims(ison_southern_women)
#' graph_dims(to_mode1(ison_southern_women))
#' @export
graph_dims <- function(object){
  if(is_twomode(object)){
    c(sum(!igraph::V(as_igraph(object))$type),
      sum(igraph::V(as_igraph(object))$type))
  } else {
    igraph::vcount(as_igraph(object))
  }
}

#' @describeIn grab Returns a vector of nodal attributes in a network
#' @importFrom igraph list.vertex.attributes
#' @examples
#' graph_node_attributes(mpn_elite_mex)
#' @export
graph_node_attributes <- function(object){
  igraph::list.vertex.attributes(as_igraph(object))
}

#' @describeIn grab Returns a vector of edge attributes in a network
#' @importFrom igraph list.edge.attributes
#' @examples
#' graph_edge_attributes(mpn_elite_mex)
#' @export
graph_edge_attributes <- function(object){
  igraph::list.edge.attributes(as_igraph(object))
}
