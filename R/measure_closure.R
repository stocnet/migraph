#' Measures of network closure
#'
#' These functions offer methods for summarising the closure in configurations 
#' in one-, two-, and three-mode networks.
#' @details 
#' For one-mode networks, shallow wrappers of igraph versions exist via 
#' `network_reciprocity` and `network_transitivity`.
#' 
#' For two-mode networks, `network_equivalency` calculates the proportion of three-paths in the network
#' that are closed by fourth tie to establish a "shared four-cycle" structure.
#' 
#' For three-mode networks, `network_congruency` calculates the proportion of three-paths 
#' spanning two two-mode networks that are closed by a fourth tie to establish a 
#' "congruent four-cycle" structure.
#' @param object A one-mode or two-mode matrix, igraph, or tidygraph
#' @param object2 Optionally, a second (two-mode) matrix, igraph, or tidygraph
#' @param method For reciprocity, either `default` or `ratio`.
#' See `?igraph::reciprocity`
#' @name closure
#' @family measures
#' @references 
#' Robins, Garry L, and Malcolm Alexander. 2004. 
#' Small worlds among interlocking directors: Network structure and distance in bipartite graphs. 
#' \emph{Computational & Mathematical Organization Theory} 10(1): 69–94.
#' \doi{10.1023/B:CMOT.0000032580.12184.c0}.
#' 
#' Knoke, David, Mario Diani, James Hollway, and Dimitris C Christopoulos. 2021. 
#' \emph{Multimodal Political Networks}. 
#' Cambridge University Press. Cambridge University Press.
#' \doi{10.1017/9781108985000}
NULL

#' @describeIn closure Calculate reciprocity in a (usually directed) network
#' @importFrom igraph reciprocity
#' @examples
#' network_reciprocity(ison_southern_women)
#' @export
network_reciprocity <- function(object, method = "default") {
  make_network_measure(igraph::reciprocity(as_igraph(object), mode = method), 
                     object)
}

#' @describeIn closure Calculate transitivity in a network
#' @importFrom igraph transitivity
#' @examples
#' network_transitivity(ison_southern_women)
#' @export
network_transitivity <- function(object) {
  make_network_measure(igraph::transitivity(as_igraph(object)), 
                     object)
}

#' @describeIn closure Calculate equivalence or reinforcement 
#'   in a (usually two-mode) network
#' @examples
#' network_equivalency(ison_southern_women)
#' @export
network_equivalency <- function(object) {
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
  make_network_measure(output, object)
}

#' @describeIn closure Calculate congruency across two two-mode networks
#' @export
network_congruency <- function(object, object2){
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
  make_network_measure(output, object)
}

