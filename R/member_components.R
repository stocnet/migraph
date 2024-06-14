#' Component partitioning algorithms
#' 
#' @description 
#'   These functions create a vector of nodes' memberships in
#'   components or degrees of coreness:
#'   
#'   - `node_in_component()` assigns nodes' component membership
#'   using edge direction where available.
#'   - `node_in_weak()` assigns nodes' component membership
#'   ignoring edge direction.
#'   - `node_in_strong()` assigns nodes' component membership
#'   based on edge direction.
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
#' @inheritParams cohesion
#' @name components
#' @family memberships
NULL

#' @rdname components 
#' @importFrom igraph components
#' @examples 
#' node_in_component(mpn_bristol)
#' @export
node_in_component <- function(.data){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  if(!manynet::is_graph(.data)) .data <- manynet::as_igraph(.data)
  make_node_member(igraph::components(.data, mode = "strong")$membership,
              .data)
}

#' @rdname components 
#' @importFrom igraph components
#' @export
node_in_weak <- function(.data){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  if(!manynet::is_graph(.data)) .data <- manynet::as_igraph(.data)
  make_node_member(igraph::components(.data, mode = "weak")$membership,
                 .data)
}

#' @rdname components 
#' @importFrom igraph components
#' @export
node_in_strong <- function(.data){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  if(!manynet::is_graph(.data)) .data <- manynet::as_igraph(.data)
  make_node_member(igraph::components(.data, mode = "strong")$membership,
                 .data)
}



