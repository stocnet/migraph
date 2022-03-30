#' Network connectedness
#' 
#' These functions return values or vectors relating to how connected a network is
#' and where the nodes or edges that would increase fragmentation are.
#' @inheritParams is
#' @name connectedness
NULL

#' @describeIn connectedness Returns number of components in the network.
#' @importFrom igraph components
#' @export
graph_components <- function(object, method = c("weak", "strong")){
  object <- as_igraph(object)
  igraph::components(object, mode = method)$no
}

#' @describeIn connectedness Returns the minimum number of nodes needed
#'   to remove from the network to increase the number of components.
#' @importFrom igraph cohesion
#' @references
#' White, Douglas R and Frank Harary 2001. 
#' The Cohesiveness of Blocks In Social Networks: Node Connectivity and Conditional Density. 
#' _Sociological Methodology_ 31 (1) : 305-359.
#' @examples 
#' graph_cohesion(ison_marvel_relationships)
#' graph_cohesion(to_main_component(ison_marvel_relationships))
#' @export
graph_cohesion <- function(object){
  igraph::cohesion(as_igraph(object))
}

#' @describeIn connectedness Returns the minimum number of edges needed
#'   to remove from the network to increase the number of components.
#' @importFrom igraph adhesion
#' @examples 
#' graph_adhesion(ison_marvel_relationships)
#' graph_adhesion(to_main_component(ison_marvel_relationships))
#' @export
graph_adhesion <- function(object){
  igraph::adhesion(as_igraph(object))
}

#' @describeIn connectedness Returns the average path length in the network.
#' @importFrom igraph mean_distance
#' @examples 
#' graph_length(ison_marvel_relationships)
#' graph_length(to_main_component(ison_marvel_relationships))
#' @export
graph_length <- function(object){
  object <- as_igraph(object)
  igraph::mean_distance(object, 
                        directed = is_directed(object))
}

#' @describeIn connectedness Returns the maximum path length in the network.
#' @importFrom igraph diameter
#' @examples 
#' graph_diameter(ison_marvel_relationships)
#' graph_diameter(to_main_component(ison_marvel_relationships))
#' @export
graph_diameter <- function(object){
  object <- as_igraph(object)
  igraph::diameter(object, 
                   directed = is_directed(object))
}

#' @describeIn connectedness Returns nodes' component membership.
#' @param method For directed networks, 
#'   either `weak` if edge direction is irrelevant,
#'   or `strong` if edge direction is salient.
#'   Ignored if network undirected. 
#' @importFrom igraph components
#' @export
node_components <- function(object, method = c("weak", "strong")){
  object <- as_igraph(object)
  igraph::components(object, mode = method)$membership
}

#' @describeIn connectedness Returns logical of which nodes cut
#'   or act as articulation points in a network.
#' @importFrom igraph articulation_points
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
  out
}

#' @describeIn connectedness Returns logical of which nodes cut
#'   or act as articulation points in a network.
#' @importFrom igraph decompose delete.edges
#' @export
edge_bridges <- function(object){
  num_comp <- length( igraph::decompose(as_igraph(object)) )
  out <- vapply(seq_len(graph_edges(object)), function(x){
    length( igraph::decompose(igraph::delete.edges(object, x)) ) > num_comp
  }, FUN.VALUE = logical(1))
  if(is_labelled(object)) 
    names(out) <- attr(igraph::E(object), "vnames")
  out
}

