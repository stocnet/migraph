#' Clustering for one-, two, and three mode networks
#'
#' This function wraps `igraph::transitivity`, and adds additional clustering
#' methods for two-mode and three-mode networks.
#' @name clustering
#' @param object A one-mode or two-mode matrix, igraph, or tidygraph
#' @param object2 Optionally, a second (two-mode) matrix, igraph, or tidygraph
#' @family two-mode functions
#' @examples
#' clustering(southern_women)
#' @export
clustering <- function(object, object2 = NULL){
  

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
  } else if (is_bipartite(object)){
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
