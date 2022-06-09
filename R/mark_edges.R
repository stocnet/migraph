#' Identify nodes and edges based on some criteria
#' 
#' These functions return logical vectors of the length of the 
#' nodes or edges in a network that identify nodes or edges that
#' meet some criteria.
#' Such functions are often useful for identifying particular
#' nodes or edges that are particularly well- or poorly-connected.
#' @inheritParams is
#' @name mark
NULL

#' @describeIn mark Returns logical of which edges are multiples
#' @importFrom igraph which_multiple
#' @examples 
#' edge_multiple(ison_marvel_relationships)
#' @export
edge_multiple <- function(object){
  object <- as_igraph(object)
  make_node_measure(igraph::which_multiple(object), object)
}

#' @describeIn mark Returns logical of which edges are loops
#' @importFrom igraph which_loop
#' @examples 
#' edge_loop(ison_marvel_relationships)
#' @export
edge_loop <- function(object){
  object <- as_igraph(object)
  make_node_measure(igraph::which_loop(object), object)
}

#' @describeIn mark Returns logical of which edges 
#'   are mutual/reciprocated
#' @importFrom igraph which_mutual
#' @examples 
#' edge_reciprocal(ison_algebra)
#' @export
edge_reciprocal <- function(object){
  object <- as_igraph(object) # allow for custom edge selection
  make_node_measure(igraph::which_mutual(object), object)
}

#' @describeIn mark Returns logical of which nodes cut
#'   or act as articulation points in a network.
#' @importFrom igraph decompose delete.edges
#' @examples 
#' edge_bridges(ison_brandes)
#' @export
edge_bridges <- function(object){
  num_comp <- length( igraph::decompose(as_igraph(object)) )
  out <- vapply(seq_len(graph_edges(object)), function(x){
    length( igraph::decompose(igraph::delete.edges(object, x)) ) > num_comp
  }, FUN.VALUE = logical(1))
  if(is_labelled(object)) 
    names(out) <- attr(igraph::E(object), "vnames")
  make_node_measure(out, object)
}

#' @describeIn mark Returns logical of which nodes cut
#'   or act as articulation points in a network.
#' @importFrom igraph articulation_points
#' @examples 
#' node_cuts(ison_brandes)
#' @export
node_cuts <- function(object){
  if(is_labelled(object)){
    out <- node_names(object) %in% 
      attr(igraph::articulation_points(as_igraph(object)), "names")
    names(out) <- node_names(object)
  } else {
    out <- 1:graph_nodes(object) %in% 
      igraph::articulation_points(as_igraph(object))
  }
  make_node_measure(out, object)
}
