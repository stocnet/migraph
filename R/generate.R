#' Create networks from particular probabilities
#' 
#' @name generate
#' @family creation
#' @param n Integer of length 1 or 2.
#'   If passed a migraph-consistent object, a random network
#'   of the same dimensions and density as the original network will be returned.
#' @param object a migraph-consistent object
#' @param p Number of edges in the network over the number of edges possible
#' @param m Number of edges in the network
#' @param directed Whether to generate network as directed. By default FALSE.
#' @details Creates a random network.
#' If `length(n)==1`, then a one-mode network will be returned,
#' equivalent to an Erdös-Renyi graph.
#' If `length(n)==1`, then a two-mode network will be returned.
#' The first number is the number of nodes in the first nodeset (rows),
#' and the second number becomes the number of nodes in the second nodeset (columns).
NULL

#' @rdname generate
#' @importFrom igraph sample_bipartite erdos.renyi.game
#' @examples
#' er1 <- autographr(generate_random(12, 0.4))
#' er2 <- autographr(generate_random(c(6, 6), 0.4))
#' grid.arrange(er1, er2, ncol = 2)
#' @export
generate_random <- function(n, p, m, directed = FALSE) {
  if(is_migraph(n)){
    m <- graph_edges(n)
    directed <- is_directed(n)
    if(is_twomode(n)){
      g <- igraph::sample_bipartite(graph_dims(n)[1], 
                                    graph_dims(n)[2],
                                    m = m, type = "gnm",
                                    directed = directed,
                                    mode = "out")
    } else {
      g <- igraph::erdos.renyi.game(graph_nodes(n), 
                                    m, type = "gnm", 
                                    directed = directed)
    }
  } else if (length(n) == 1) {
    type <- ifelse(missing(p) || is.null(p), "gnm", "gnp")
    if (missing(p) || is.null(p)) p <- m
    g <- igraph::erdos.renyi.game(n, 
                                  p, type = type, 
                                  directed = directed)
  } else if (length(n) == 2) {
    type <- ifelse(missing(p) || is.null(p), "gnm", "gnp")
    g <- igraph::sample_bipartite(n[1],
                                  n[2],
                                  p = p,
                                  m = m,
                                  type = type,
                                  directed = directed,
                                  mode = "out")
  } else {
    stop("`n` must be of length=1 for a one-mode network or length=2 for a two-mode network.")
  }
  g
}

#' @rdname generate
#' @importFrom igraph sample_smallworld
#' @examples
#' sw1 <- autographr(generate_smallworld(12, 0.025))
#' sw2 <- autographr(generate_smallworld(12, 0.25))
#' grid.arrange(sw1, sw2, ncol = 2)
#' @export
generate_smallworld <- function(n, p = 0.05) {
  igraph::sample_smallworld(dim = 1, size = n, 
                            nei = 2, p = p)
}

#' @rdname generate
#' @importFrom igraph sample_pa
#' @examples
#' sf1 <- autographr(generate_scalefree(12, 0.25))
#' sf2 <- autographr(generate_scalefree(12, 1.25))
#' grid.arrange(sf1, sf2, ncol = 2)
#' @export
generate_scalefree <- function(n, p = 1) {
  igraph::sample_pa(n, power = p)
}

#' @rdname generate
#' @examples
#' em1 <- autographr(mpn_elite_usa_advice)
#' em2 <- autographr(generate_permutation(mpn_elite_usa_advice))
#' grid.arrange(em1, em2, ncol = 2)
#' @export
generate_permutation <- function(object) {
  if(is_twomode(object)){
    out <- r2perm(as_matrix(object))
  } else {
    out <- r1perm(as_matrix(object))
  }
  out <- copy_node_attributes(out, object)
  out
}

r1perm <- function(m) {
  n <- sample(1:dim(m)[1])
  if(is_labelled(m)){
    p <- matrix(data = m[n, n], nrow = dim(m)[1], ncol = dim(m)[2],
                dimnames = dimnames(m))
  } else {
    p <- matrix(data = m[n, n], nrow = dim(m)[1], ncol = dim(m)[2])
  }
  p
}

r2perm <- function(m) {
  n <- sample(1:dim(m)[1])
  o <- sample(1:dim(m)[2])
  if(is_labelled(m)){
    p <- matrix(data = m[n, o], nrow = dim(m)[1], ncol = dim(m)[2],
                dimnames = dimnames(m))
  } else {
    p <- matrix(data = m[n, o], nrow = dim(m)[1], ncol = dim(m)[2])
  }
  p
}


# igraph::grg.game()
# igraph::sbm.game()
# igraph::hrg.game()
# igraph::aging.ba.game()