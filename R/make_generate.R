#' Create networks from particular probabilities
#' 
#' @description These functions are similar to the `create_*` functions,
#'   but include some random element. They are particularly useful for
#'   creating a distribution of networks for exploring or testing
#'   network properties.
#' @name generate
#' @family creation
#' @inheritParams create
#' @param object a migraph-consistent object
#' @param p Proportion of possible edges in the network that are realised or,
#'   if integer greater than 1, the number of edges in the network.
#' @param directed Whether to generate network as directed. By default FALSE.
NULL

#' @describeIn generate Generates a random network with a particular probability.
#' @importFrom igraph sample_bipartite sample_gnp sample_gnm
#' @examples
#' autographr(generate_random(12, 0.4)) +
#' autographr(generate_random(c(6, 6), 0.4))
#' @export
generate_random <- function(n, p = 0.5, directed = FALSE) {
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
      g <- igraph::sample_gnm(graph_nodes(n), 
                                    m = m,
                                    directed = directed)
    }
  } else if (length(n) == 1) {
    if(p > 1){
      if(!is.integer(p)) stop("`p` must be an integer if above 1.")
      g <- igraph::sample_gnm(n, m = p, directed = directed)
    } else {
      g <- igraph::sample_gnp(n, p = p, directed = directed)
    }
  } else if (length(n) == 2) {
    if(p > 1){
      if(!is.integer(p)) stop("`p` must be an integer if above 1.")
      g <- igraph::sample_bipartite(n[1], n[2],
                                    m = p,
                                    type = "gmp",
                                    directed = directed,
                                    mode = "out")
    } else {
      g <- igraph::sample_bipartite(n[1], n[2],
                                    p = p,
                                    type = "gnp",
                                    directed = directed,
                                    mode = "out")
    }
    
  } else {
    stop("`n` must be of length=1 for a one-mode network or length=2 for a two-mode network.")
  }
  g
}

#' @describeIn generate Generates a small-world structure
#'   following the lattice rewiring model.
#' @importFrom igraph sample_smallworld
#' @examples
#' autographr(generate_smallworld(12, 0.025)) +
#' autographr(generate_smallworld(12, 0.25))
#' @export
generate_smallworld <- function(n, p = 0.05) {
  igraph::sample_smallworld(dim = 1, size = n, 
                            nei = 2, p = p)
}

#' @describeIn generate Generates a scale-free structure
#'   following the preferential attachment model.
#' @importFrom igraph sample_pa
#' @examples
#' autographr(generate_scalefree(12, 0.25)) +
#' autographr(generate_scalefree(12, 1.25))
#' @export
generate_scalefree <- function(n, p = 1) {
  igraph::sample_pa(n, power = p)
}

#' @describeIn generate Generates a permutation of the original network
#'   using a Fisher-Yates shuffle on both the rows and columns (for a one-mode network)
#'   or on each of the rows and columns (for a two-mode network).
#' @param with_attr Logical. Whether any attributes of the object
#'   should be retained. By default TRUE. 
#' @examples
#' autographr(mpn_elite_usa_advice) +
#' autographr(generate_permutation(mpn_elite_usa_advice))
#' @export
generate_permutation <- function(object, with_attr = TRUE) {
  out <- as_matrix(object)
  if(is_twomode(object)){
    out <- r2perm(out)
  } else {
    out <- r1perm(out)
  }
  if(with_attr) out <- copy_node_attributes(out, object)
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