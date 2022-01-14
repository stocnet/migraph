#' Identifying edges by certain properties
#' @inheritParams is
#' @name edge
NULL

#' @describeIn edge Identify edges that are mutual/reciprocated
#' @importFrom igraph which_mutual
#' @examples 
#' edge_mutual(ison_m182)
#' @export
edge_mutual <- function(object){
  object <- as_igraph(object) # allow for custom edge selection
  igraph::which_mutual(object)
}

#' @describeIn edge Identify edges that are multiples
#' @importFrom igraph which_multiple
#' @examples 
#' edge_multiple(ison_m182)
#' @export
edge_multiple <- function(object){
  object <- as_igraph(object)
  igraph::which_multiple(object)
}

#' @describeIn edge Identify edges that are loops
#' @importFrom igraph which_loop
#' @examples 
#' edge_loop(ison_m182)
#' @export
edge_loop <- function(object){
  object <- as_igraph(object)
  igraph::which_loop(object)
}

#' @describeIn edge Calculate number of shortest paths going through an edge
#' @importFrom igraph estimate_edge_betweenness
#' @examples
#' edge_betweenness(ison_coleman)
#' @export
edge_betweenness <- function(object) {
  object <- as_igraph(object)
  igraph::estimate_edge_betweenness(object)
}

#' @describeIn edge Calculate the lengths of the shortest paths
#' @importFrom igraph shortest_paths
#' @examples
#' edge_closeness(ison_coleman)
#' @export
# edge_closeness <- function(object) {
#   object <- as_igraph(object)
#   
#   out
# }

