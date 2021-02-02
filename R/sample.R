#' Sample networks according to certain probabilities
#' 
#' @name sample
#' @family creation
#' @param n1 Number of nodes in the first node set
#' @param n2 Number of nodes in the second node set
#' @param p Number of edges in the network over the number of edges possible
#' @param m Number of edges in the network
#' @param directed Should direction of edges be used for the calculations
#' @param mode How should edges be followed
#' @param as What type of object to return.
#' One of "matrix", "tidygraph", "igraph".
#' By default, creates tidygraph's "tbl_graph" object.
#' @details Creates a random two-mode network.
#' Will construct an affiliation matrix,
#' with a certain probability of a tie.
#' @importFrom tidygraph play_bipartite
#' @importFrom igraph as.igraph as_adjacency_matrix
#' @examples
#' sample_affiliation(10, 12, 0.25) %>% ggraph::ggraph() +
#' ggraph::geom_edge_fan(ggplot2::aes(alpha = stat(index)), show.legend = FALSE) +
#' ggraph::geom_node_point(ggplot2::aes(size = 5))
#' @export
sample_affiliation <- function(n1, n2, p, m, directed = TRUE, mode = "out",
                         as = c("tidygraph", "igraph", "matrix")) {
  
  g <- tidygraph::play_bipartite(n1, n2, p, m, directed, mode)
  
  as <- match.arg(as)
  if(as == "igraph" | as == "matrix") g <- igraph::as.igraph(g)
  if(as == "matrix") g <- igraph::as_adjacency_matrix(g)
  g
}
