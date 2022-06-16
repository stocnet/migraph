#' Network connectedness
#' 
#' These functions return values or vectors relating to how connected a network is
#' and where the nodes or edges that would increase fragmentation are.
#' @inheritParams is
#' @name connectedness
#' @family measures
NULL

#' @describeIn connectedness Returns number of (strong) components in the network.
#'   To get the 'weak' components of a directed graph, 
#'   please use `to_undirected()` first.
#' @importFrom igraph components
#' @export
graph_components <- function(object){
  object <- as_igraph(object)
  make_graph_measure(igraph::components(object, mode = "strong")$no,
                     object)
}

#' @describeIn connectedness Returns the minimum number of nodes to remove
#'   from the network needed to increase the number of components.
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
  make_graph_measure(igraph::cohesion(as_igraph(object)),
                     object)
}

#' @describeIn connectedness Returns the minimum number of edges needed
#'   to remove from the network to increase the number of components.
#' @importFrom igraph adhesion
#' @examples 
#' graph_adhesion(ison_marvel_relationships)
#' graph_adhesion(to_main_component(ison_marvel_relationships))
#' @export
graph_adhesion <- function(object){
  make_graph_measure(igraph::adhesion(as_igraph(object)),
                     object)
}

#' @describeIn connectedness Returns the maximum path length in the network.
#' @importFrom igraph diameter
#' @examples 
#' graph_diameter(ison_marvel_relationships)
#' graph_diameter(to_main_component(ison_marvel_relationships))
#' @export
graph_diameter <- function(object){
  object <- as_igraph(object)
  make_graph_measure(igraph::diameter(object, 
                   directed = is_directed(object)),
                   object)
}

#' @describeIn connectedness Returns the average path length in the network.
#' @importFrom igraph mean_distance
#' @examples 
#' graph_length(ison_marvel_relationships)
#' graph_length(to_main_component(ison_marvel_relationships))
#' @export
graph_length <- function(object){
  object <- as_igraph(object)
  make_graph_measure(igraph::mean_distance(object, 
                        directed = is_directed(object)),
                     object)
}

#' @describeIn mark Returns logical of which nodes cut
#'   or act as articulation points in a network,
#'   increasing the number of connected components in a graph when removed.
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

