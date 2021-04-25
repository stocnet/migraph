#' Create networks from particular probabilities
#' 
#' @name generate
#' @family creation
#' @param n Integer of length 1 or 2.
#' @param p Number of edges in the network over the number of edges possible
#' @param m Number of edges in the network
#' @details Creates a random network.
#' If `length(n)==1`, then a one-mode network will be returned,
#' equivalent to an Erdös-Renyi graph.
#' If `length(n)==1`, then a two-mode network will be returned.
#' The first number is the number of nodes in the first nodeset (rows),
#' and the second number becomes the number of nodes in the second nodeset (columns).
#' @importFrom igraph sample_bipartite
#' @examples
#' plot(generate_random(c(10, 12), 0.25))
#' @export
generate_random <- function(n, p, m) {
  
  if(length(n)==1){
    type <- ifelse(is.null(p), "gnm", "gnp")
    if(is.null(p)) p <- m
    g <- igraph::erdos.renyi.game(n, p, type = type)
  } else if (length(n)==2){
    type <- ifelse(is.null(p), "gnm", "gnp")
    g <- igraph::sample_bipartite(n[1], n[2], p = p, m = m, directed = FALSE, mode = "out")
  } else stop("`n` must be of length=1 for a one-mode network or length=2 for a two-mode network.")
  g
}

# igraph::ba.game()
# igraph::grg.game()
# igraph::sbm.game()
# igraph::hrg.game()
# igraph::aging.ba.game()