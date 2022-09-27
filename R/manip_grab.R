#' Grab various node or edge attributes from a network
#' 
#' @description These functions operate to help extract certain attributes
#'   from given network data.
#'   They are also useful as helpers within other functions.
#'   
#'   `network_*()` functions always relate to the overall graph or network,
#'   usually returning a scalar.
#'   `node_*()` and `tie_*()` always return vectors the same length
#'   as the number of nodes or edges in the network, respectively.
#' @name grab
#' @family manipulations
#' @inheritParams is
#' @param attribute Character string naming an attribute in the object.
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
  if(is_twomode(object)){
    out <- igraph::get.vertex.attribute(as_igraph(object), "type")
  } else{
    out <- rep(FALSE, network_nodes(object))
  }
  # cannot use make_node_mark here because then eternal loop
  class(out) <- c("node_mark", class(out))
  if(is.null(names(out)) & is_labelled(object))
    names(out) <- node_names(object)
  attr(out, "mode") <- out
  out
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
#' tie_attribute(ison_algebra, "task_tie")
#' @export
tie_attribute <- function(object, attribute){
  igraph::get.edge.attribute(as_igraph(object), attribute)
}

#' @describeIn grab Extracts the weights of the edges in a network.
#' @examples
#' tie_weights(to_mode1(ison_southern_women))
#' @export
tie_weights <- function(object){
  object <- as_igraph(object)
  out <- igraph::get.edge.attribute(object, "weight")
  if(is.null(out)) out <- rep(1, network_ties(object))
  out
}

#' @describeIn grab Extracts the signs of the edges in a network.
#' @examples 
#' tie_signs(ison_marvel_relationships)
#' @export
tie_signs <- function(object){
  object <- as_igraph(object)
  out <- igraph::get.edge.attribute(object, "sign")
  if(is.null(out)) out <- rep(1, network_ties(object))
  out
}

#' @describeIn grab Returns the total number of nodes (of any mode) in a network.
#' @examples
#' network_nodes(ison_southern_women)
#' @export
network_nodes <- function(object){
  igraph::vcount(as_igraph(object))
}

#' @describeIn grab Returns the number of edges in a network.
#' @examples
#' network_ties(ison_southern_women)
#' @export
network_ties <- function(object){
  igraph::ecount(as_igraph(object))
}

#' @describeIn grab Returns the dimensions of a network in a vector
#'   as long as the number of modes in the network.
#' @examples
#' network_dims(ison_southern_women)
#' network_dims(to_mode1(ison_southern_women))
#' @export
network_dims <- function(object){
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
#' network_node_attributes(mpn_elite_mex)
#' @export
network_node_attributes <- function(object){
  igraph::list.vertex.attributes(as_igraph(object))
}

#' @describeIn grab Returns a vector of edge attributes in a network
#' @importFrom igraph list.edge.attributes
#' @examples
#' network_tie_attributes(mpn_elite_mex)
#' @export
network_tie_attributes <- function(object){
  igraph::list.edge.attributes(as_igraph(object))
}
