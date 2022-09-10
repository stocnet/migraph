#' Measures of network cohesion or connectedness
#' 
#' These functions return values or vectors relating to how connected a network is
#' and the number of nodes or edges to remove that would increase fragmentation.
#' @inheritParams is
#' @name cohesion
#' @family measures
NULL

#' @describeIn cohesion summarises the ratio of ties
#' to the number of possible ties.
#' @importFrom igraph edge_density
#' @examples 
#' graph_density(mpn_elite_mex)
#' graph_density(mpn_elite_usa_advice)
#' @export
graph_density <- function(object) {
  if (is_twomode(object)) {
    mat <- as_matrix(object)
    out <- sum(mat) / (nrow(mat) * ncol(mat))
  } else {
    out <- igraph::edge_density(as_igraph(object))
  }
  make_graph_measure(out, object)
}

#' @describeIn cohesion Returns number of (strong) components in the network.
#'   To get the 'weak' components of a directed graph, 
#'   please use `to_undirected()` first.
#' @importFrom igraph components
#' @export
graph_components <- function(object){
  object <- as_igraph(object)
  make_graph_measure(igraph::components(object, mode = "strong")$no,
                     object)
}

#' @describeIn cohesion Returns the minimum number of nodes to remove
#'   from the network needed to increase the number of components.
#' @importFrom igraph cohesion
#' @references
#' White, Douglas R and Frank Harary. 2001. 
#' "The Cohesiveness of Blocks In Social Networks: Node Connectivity and Conditional Density." 
#' _Sociological Methodology_ 31(1): 305-59.
#' @examples 
#' graph_cohesion(ison_marvel_relationships)
#' graph_cohesion(to_giant(ison_marvel_relationships))
#' @export
graph_cohesion <- function(object){
  make_graph_measure(igraph::cohesion(as_igraph(object)),
                     object)
}

#' @describeIn cohesion Returns the minimum number of edges needed
#'   to remove from the network to increase the number of components.
#' @importFrom igraph adhesion
#' @examples 
#' graph_adhesion(ison_marvel_relationships)
#' graph_adhesion(to_giant(ison_marvel_relationships))
#' @export
graph_adhesion <- function(object){
  make_graph_measure(igraph::adhesion(as_igraph(object)),
                     object)
}

#' @describeIn cohesion Returns the maximum path length in the network.
#' @importFrom igraph diameter
#' @examples 
#' graph_diameter(ison_marvel_relationships)
#' graph_diameter(to_giant(ison_marvel_relationships))
#' @export
graph_diameter <- function(object){
  object <- as_igraph(object)
  make_graph_measure(igraph::diameter(object, 
                   directed = is_directed(object)),
                   object)
}

#' @describeIn cohesion Returns the average path length in the network.
#' @importFrom igraph mean_distance
#' @examples 
#' graph_length(ison_marvel_relationships)
#' graph_length(to_giant(ison_marvel_relationships))
#' @export
graph_length <- function(object){
  object <- as_igraph(object)
  make_graph_measure(igraph::mean_distance(object, 
                        directed = is_directed(object)),
                     object)
}
