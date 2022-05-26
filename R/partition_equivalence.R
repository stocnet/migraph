#' Clustering algorithms
#' 
#' These functions combine an appropriate `_census()` function
#' together with methods for calculating the hierarchical clusters
#' provided by a certain distance calculation.
#' @name cluster
#' @inheritParams is
#' @importFrom stats as.dist hclust
NULL

#' @rdname cluster
#' @examples
#' ggtree(cluster_structural_equivalence(mpn_elite_mex))
#' node_structural_equivalence(mpn_elite_mex)
#' @export
node_structural_equivalence <- function(object, 
                                        method = c("strict", "elbow")){
  mat <- node_tie_census(object)
  if(any(colSums(t(mat))==0)){
    mat <- cbind(mat, (colSums(t(mat))==0))
  } 
  correlations <- cor(t(mat))
  dissimilarity <- 1 - correlations
  distances <- stats::as.dist(dissimilarity)
  hc <- stats::hclust(distances)
  
  method <- match.arg(method)
  if(method == "strict") k <- k_strict(hc, object)
  # out <- clusters
  # out
  # select_clusters(clusters, mat, method = method)
  make_partition(cutree(hc, k), object)
}

#' @rdname cluster
#' @examples
#' ggtree(cluster_regular_equivalence(mpn_elite_mex))
#' ggtree(cluster_regular_equivalence(mpn_elite_usa_advice))
#' node_regular_equivalence(mpn_elite_mex)
#' @export
node_regular_equivalence <- function(object, 
                                     method = c("strict", "elbow")){
  if(is_twomode(object)){
    triads <- as.matrix(node_quad_census(object))
  } else {
    triads <- node_triad_census(object)
  }
  if(any(colSums(triads) == 0)) triads <- triads[,-which(colSums(triads) == 0)]
  correlations <- cor(t(triads))
  dissimilarity <- 1 - correlations
  distances <- stats::as.dist(dissimilarity)
  hc <- stats::hclust(distances)
  
  method <- match.arg(method)
  if(method == "strict") k <- k_strict(hc, object)
  make_partition(cutree(hc, k), object)
}

k_strict <- function(hc, object){
  zero_merged <- hc$merge[hc$height == 0,]
  k <- graph_nodes(object) - sum(zero_merged < 0) + sum(zero_merged > 0)
}

k_elbow <- function(){
  
}

select_clusters <- function(hc, census, method = c("elbow", "strict")){
  vertices <- nrow(census)
  observedcorrelation <- cor(t(census))
  method <- match.arg(method)
  
  resultlist <- list()
  correlations <- vector()
  for (i in 2:(vertices)) {
    cluster_result <- list(label = NA, clusters = NA, correlation = NA)
    cluster_result$label <- paste("number of clusters: ", 
                                  i)
    clusters <- stats::cutree(hc, k = i)
    cluster_result$clusters <- clusters
    cluster_cor_mat <- clusterCorr(observedcorrelation, clusters)
    clustered_observed_cors <- sna::gcor(cluster_cor_mat, observedcorrelation)
    cluster_result$correlation <- (clustered_observed_cors)
    resultlist <- c(resultlist, cluster_result)
    correlations <- c(correlations, clustered_observed_cors)
  }
  
  resultlist$correlations <- c(correlations)
  dafr <- data.frame(clusters = 2:vertices, correlations = c(correlations))
  # resultlist
  correct <- NULL # to satisfy the error god
  
  # k identification method
  if(method == "elbow"){
    dafr$correct <- ifelse(dafr$clusters == elbow_finder(dafr$clusters, dafr$correlations),
                           "#E20020", "#6f7072")
  } else if (method == "strict"){
    dafr$correct <- "#6f7072"
    dafr$correct[which(elementwise.all.equal(dafr$correlations, 1))[1]] <- "#E20020"
  } else stop("This k selection method is not recognised")
  
  dafr
}  

clusterCorr <- function(observed_cor_matrix, cluster_vector) {
  num_vertices = nrow(observed_cor_matrix)
  cluster_cor_mat <- observed_cor_matrix
  
  obycor <- function(i, j) mean(observed_cor_matrix[which(cluster_vector[row(observed_cor_matrix)] ==
                                                            cluster_vector[i] &
                                                            cluster_vector[col(observed_cor_matrix)] ==
                                                            cluster_vector[j])])
  obycor_v <- Vectorize(obycor)
  cluster_cor_mat <- outer(1:num_vertices,
                           1:num_vertices,
                           obycor_v)
  dimnames(cluster_cor_mat) <- dimnames(observed_cor_matrix)
  cluster_cor_mat
}
