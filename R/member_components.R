#' Component partitioning algorithms
#' 
#' @description 
#'   These functions create a vector of nodes' memberships in
#'   components or degrees of coreness.
#'   
#'   In graph theory, components, sometimes called connected components, 
#'   are induced subgraphs from partitioning the nodes into disjoint sets.
#'   All nodes that are members of the same partition as _i_ are reachable
#'   from _i_.
#'   
#'   For directed networks, 
#'   strongly connected components consist of subgraphs where there are paths
#'   in each direction between member nodes.
#'   Weakly connected components consist of subgraphs where there is a path
#'   in either direction between member nodes.
#'   
#'   Coreness captures the maximal subgraphs in which each vertex has at least
#'   degree _k_, where _k_ is also the order of the subgraph.
#'   As described in `igraph::coreness`,
#'   a node's coreness is _k_ if it belongs to the _k_-core
#'   but not to the (_k_+1)-core.
#' @inheritParams is
#' @name components
#' @family membership
NULL

#' @describeIn components Returns nodes' component membership
#'   using edge direction where available.
#' @importFrom igraph components
#' @examples 
#' node_components(mpn_bristol)
#' @export
node_components <- function(object){
  if(!is_graph(object)) object <- as_igraph(object)
  make_node_member(igraph::components(object, mode = "strong")$membership,
              object)
}

#' @describeIn components Returns nodes' component membership
#'   ignoring edge direction.
#' @importFrom igraph components
#' @export
node_weak_components <- function(object){
  if(!is_graph(object)) object <- as_igraph(object)
  make_node_member(igraph::components(object, mode = "weak")$membership,
                 object)
}

#' @describeIn components Returns nodes' component membership
#'   based on edge direction.
#' @importFrom igraph components
#' @export
node_strong_components <- function(object){
  if(!is_graph(object)) object <- as_igraph(object)
  make_node_member(igraph::components(object, mode = "strong")$membership,
                 object)
}

#' @describeIn components Returns k-cores
#' @examples
#' node_coreness(ison_adolescents)
#' @export
node_coreness <- function(object){
  if(!is_graph(object)) object <- as_igraph(object)
  out <- igraph::coreness(object)
  make_node_member(out, object)
}

