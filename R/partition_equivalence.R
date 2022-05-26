#' Clustering algorithms
#' 
#' These functions combine an appropriate `_census()` function
#' together with methods for calculating the hierarchical clusters
#' provided by a certain distance calculation.
#' @name cluster
#' @param object A migraph-consistent object.
#' @importFrom stats as.dist hclust
NULL

#' @rdname cluster
#' @examples
#' ggtree(cluster_structural_equivalence(mpn_elite_mex))
#' @export
cluster_structural_equivalence <- function(object){
  mat <- node_tie_census(object)
  if(any(colSums(t(mat))==0)){
    mat <- cbind(mat, (colSums(t(mat))==0))
  } 
  correlations <- cor(t(mat))
  dissimilarity <- 1 - correlations
  distances <- stats::as.dist(dissimilarity)
  clusters <- stats::hclust(distances)
  clusters
}

#' @rdname cluster
#' @examples
#' ggtree(cluster_regular_equivalence(mpn_elite_mex))
#' ggtree(cluster_regular_equivalence(mpn_elite_usa_advice))
#' @export
cluster_regular_equivalence <- function(object){
  if(is_twomode(object)){
    triads <- as.matrix(node_quad_census(object))
  } else {
    triads <- node_triad_census(object)
  }
  if(any(colSums(triads) == 0)) triads <- triads[,-which(colSums(triads) == 0)]
  correlations <- cor(t(triads))
  dissimilarity <- 1 - correlations
  distances <- stats::as.dist(dissimilarity)
  clusters <- stats::hclust(distances)
  clusters
}

