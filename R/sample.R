#' Sample networks according to certain probabilities
#' 
#' @name sample
#' @family creation
#' @param n Integer vector of length 2. 
#' The first number is the number of nodes in the first nodeset (rows),
#' and the second number becomes the number of nodes in the second nodeset (columns).
#' @param p Number of edges in the network over the number of edges possible
#' @param m Number of edges in the network
#' @details Creates a random two-mode network.
#' Will construct an affiliation matrix,
#' with a certain probability of a tie.
#' @importFrom igraph sample_bipartite
#' @examples
#' plot(sample_affiliation(c(10, 12), 0.25))
#' @export
sample_affiliation <- function(n, p, m) {
  
  type <- ifelse(is.null(p), "gnm", "gnp")
  g <- igraph::sample_bipartite(n[1], n[2], p = p, m = m, directed = FALSE, mode = "out")
  g
}
