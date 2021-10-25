#' Clustering algorithms
#' 
#' These functions combine an appropriate `_census()` function
#' together with methods for calculating the hierarchical clusters
#' provided by a certain distance calculation.
#' @name cluster
#' @param object a migraph-consistent object
#' @importFrom stats as.dist hclust
NULL

#' @rdname cluster
#' @examples
#' ggtree(cluster_structural_equivalence(mpn_elite_mex))
#' @export
cluster_structural_equivalence <- function(object){
  mat <- node_tie_census(object)
  correlations <- cor(t(mat))
  dissimilarity <- 1 - correlations
  distances <- stats::as.dist(dissimilarity)
  clusters <- stats::hclust(distances)
  clusters
}

#' @rdname cluster
#' @examples
#' ggtree(cluster_regular_equivalence(mpn_elite_mex))
#' @export
cluster_regular_equivalence <- function(object){
  triads <- node_triad_census(object)
  triads <- triads[,-which(colSums(triads) == 0)]
  correlations <- cor(t(triads))
  dissimilarity <- 1 - correlations
  distances <- stats::as.dist(dissimilarity)
  clusters <- stats::hclust(distances)
  clusters
}

