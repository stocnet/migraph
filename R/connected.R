#' Network connectedness
#' @param object a migraph-consistent object
#' @name connectedness
NULL

#' @describeIn connectedness Returns nodes' component membership
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

#' @describeIn connectedness Returns number of components in the network
#' @importFrom igraph components
#' @export
graph_components <- function(object, method = c("weak", "strong")){
  object <- as_igraph(object)
  igraph::components(object, mode = method)$no
}

#' @describeIn connectedness Returns logical of which nodes cut
#'   or act as articulation points in a network
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
#'   or act as articulation points in a network
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

#' @rdname connectedness
#' @importFrom igraph cohesion
#' @examples 
#' graph_cohesion(ison_marvel_relationships)
#' graph_cohesion(to_main_component(ison_marvel_relationships))
#' @export
graph_cohesion <- function(object){
  igraph::cohesion(as_igraph(object))
}

#' @rdname connectedness
#' @importFrom igraph adhesion
#' @examples 
#' graph_adhesion(ison_marvel_relationships)
#' graph_adhesion(to_main_component(ison_marvel_relationships))
#' @export
graph_adhesion <- function(object){
  igraph::adhesion(as_igraph(object))
}

#' @rdname connectedness
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

#' @rdname connectedness
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

