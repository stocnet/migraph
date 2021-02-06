#' Clustering for one-, two-, and three- mode networks
#'
#' This function offers clustering methods for one-, two-, and three-mode networks.
#' 
#' For one-mode networks, the function serves as a shallow wrapper for `igraph::transitivity`,
#' since global transitivity is a regular measure for clustering or local density in one-mode networks.
#' 
#' For two-mode networks, we calculate the proportion of three-paths in the network
#' that are closed by fourth tie to establish a "shared four-cycle" structure.
#' 
#' For three-mode networks, we calculate the proportion of three-paths spanning the two two-mode networks
#' that are closed by a fourth tie to establish a "congruent four-cycle" structure.
#' @param object A one-mode or two-mode matrix, igraph, or tidygraph
#' @param object2 Optionally, a second (two-mode) matrix, igraph, or tidygraph
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
#' @examples
#' graph_clustering(southern_women)
#' @export
graph_clustering <- function(object, object2 = NULL){
  
  if(!is.null(object2)){ # run three-mode clustering
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
      (sum(twopaths * (twopaths - 1)) + sum(twopaths *
                                              (matrix(degrees, c, c) - twopaths)))
    if(is.nan(output)) output <- 1
  } else if (is_twomode(object)){
    mat <- as_matrix(object)
    c <- ncol(mat)
    indegrees <- colSums(mat)
    twopaths <- crossprod(mat)
    diag(twopaths) <- 0
    output <- sum(twopaths * (twopaths - 1)) /
      (sum(twopaths * (twopaths - 1)) + sum(twopaths *
                                              (matrix(indegrees, c, c) - twopaths)))
    if(is.nan(output)) output <- 1
  } else {
    graph <- as_igraph(object)
    output <- igraph::transitivity(graph)
  }
  output
}
