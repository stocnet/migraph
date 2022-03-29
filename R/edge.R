#' Identifying edges by certain properties
#' @inheritParams is
#' @name edge
NULL

#' @describeIn edge Identify edges that are mutual/reciprocated
#' @importFrom igraph which_mutual
#' @examples 
#' edge_mutual(ison_algebra)
#' @export
edge_mutual <- function(object){
  object <- as_igraph(object) # allow for custom edge selection
  igraph::which_mutual(object)
}

#' @describeIn edge Identify edges that are multiples
#' @importFrom igraph which_multiple
#' @examples 
#' edge_multiple(ison_algebra)
#' @export
edge_multiple <- function(object){
  object <- as_igraph(object)
  igraph::which_multiple(object)
}

#' @describeIn edge Identify edges that are loops
#' @importFrom igraph which_loop
#' @examples 
#' edge_loop(ison_algebra)
#' @export
edge_loop <- function(object){
  object <- as_igraph(object)
  igraph::which_loop(object)
}

#' @describeIn edge Calculate number of shortest paths going through an edge
#' @importFrom igraph edge_betweenness
#' @examples
#' (eb <- edge_betweenness(ison_adolescents))
#' plot(eb)
#' ison_adolescents %>% 
#'   activate(edges) %>% mutate(weight = eb) %>% 
#'   autographr()
#' @export
edge_betweenness <- function(object){
  object <- as_igraph(object)
  edges <- as_edgelist(object)
  edges <- paste(edges$from, edges$to, sep = "-")
  out <- igraph::edge_betweenness(object)
  names(out) <- edges
  class(out) <- c("measure", class(out))
  out
}

#' @describeIn edge Calculate the closeness of each edge to each other edge
#' in the network.
#' @examples
#' (ec <- edge_closeness(ison_adolescents))
#' plot(ec)
#' ison_adolescents %>% 
#'   activate(edges) %>% mutate(weight = ec) %>% 
#'   autographr()
#' @export
edge_closeness <- function(object){
  edge_adj <- to_edges(object)
  node_closeness(edge_adj)
}

