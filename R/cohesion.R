#' Cohesion for one-, two-, and three- mode networks
#'
#' These functions offer methods for summarising the cohesion in one-, two-, and three-mode networks.
#' @details 
#' For one- and two-mode networks, `graph_density` summarises the ratio of ties
#' to the number of possible ties.
#' 
#' For one-mode networks, shallow wrappers of igraph versions exist via 
#' `graph_reciprocity` and `graph_transitivity`.
#' 
#' For two-mode networks, `graph_equivalency` calculates the proportion of three-paths in the network
#' that are closed by fourth tie to establish a "shared four-cycle" structure.
#' 
#' For three-mode networks, `graph_congruency` calculates the proportion of three-paths spanning the two two-mode networks
#' that are closed by a fourth tie to establish a "congruent four-cycle" structure.
#' @param object A one-mode or two-mode matrix, igraph, or tidygraph
#' @param object2 Optionally, a second (two-mode) matrix, igraph, or tidygraph
#' @param method For reciprocity, either `default` or `ratio`.
#' See `?igraph::reciprocity`
#' @name cohesion
#' @family one-mode measures
#' @family two-mode measures
#' @family three-mode measures
#' @references 
#' Robins, Garry L, and Malcolm Alexander. 2004. 
#' Small worlds among interlocking directors: Network structure and distance in bipartite graphs. 
#' \emph{Computational & Mathematical Organization Theory} 10 (1): 69–94.
#' 
#' Knoke, David, Mario Diani, James Hollway, and Dimitris C Christopoulos. 2021. 
#' \href{https://www.cambridge.org/core/books/multimodal-political-networks/43EE8C192A1B0DCD65B4D9B9A7842128}{\emph{Multimodal Political Networks}}. 
#' Cambridge University Press. Cambridge University Press.
NULL

#' @rdname cohesion
#' @importFrom igraph edge_density
#' @examples 
#' graph_density(mpn_elite_mex)
#' graph_density(mpn_elite_usa_advice)
#' @export
graph_density <- function(object) {
  if (is_twomode(object)) {
    mat <- as_matrix(object)
    sum(mat) / (nrow(mat) * ncol(mat))
  } else {
    igraph::edge_density(as_igraph(object))
  }
}
#' @rdname cohesion
#' @importFrom igraph reciprocity
#' @examples
#' graph_reciprocity(ison_southern_women)
#' @export
graph_reciprocity <- function(object, method = "default") {
  igraph::reciprocity(as_igraph(object), mode = method)
}
#' @rdname cohesion
#' @importFrom igraph transitivity
#' @examples
#' graph_transitivity(ison_southern_women)
#' @export
graph_transitivity <- function(object) {
  igraph::transitivity(as_igraph(object))
}

#' @rdname cohesion
#' @examples
#' graph_equivalency(ison_southern_women)
#' @export
graph_equivalency <- function(object) {
  if (is_twomode(object)) {
    mat <- as_matrix(object)
    c <- ncol(mat)
    indegrees <- colSums(mat)
    twopaths <- crossprod(mat)
    diag(twopaths) <- 0
    output <- sum(twopaths * (twopaths - 1)) /
      (sum(twopaths * (twopaths - 1)) +
         sum(twopaths *
             (matrix(indegrees, c, c) - twopaths)))
    if (is.nan(output)) output <- 1
  } else stop("This function expects a two-mode network")
  output
}

#' @rdname cohesion
#' @export
graph_congruency <- function(object, object2){
  if(missing(object) | missing(object2)) stop("This function expects two two-mode networks")
  mat1 <- as_matrix(object)
  mat2 <- as_matrix(object2)
  c <- ncol(mat1)
  twopaths1 <- crossprod(mat1)
  indegrees <- diag(twopaths1)
  diag(twopaths1) <- 0
  twopaths2 <- tcrossprod(mat2)
  outdegrees <- diag(twopaths2)
  diag(twopaths2) <- 0
  twopaths <- twopaths1 + twopaths2
  degrees <- indegrees + outdegrees
  output <- sum(twopaths * (twopaths - 1)) /
    (sum(twopaths * (twopaths - 1)) +
       sum(twopaths *
             (matrix(degrees, c, c) - twopaths)))
  if (is.nan(output)) output <- 1
  output
}

