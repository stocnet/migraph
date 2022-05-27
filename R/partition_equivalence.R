#' Equivalence clustering algorithms
#' 
#' @description 
#'   These functions combine an appropriate `_census()` function
#'   together with methods for calculating the hierarchical clusters
#'   provided by a certain distance calculation.
#'   
#'   A `plot()` method exists for investigating the dendrogram
#'   of the hierarchical cluster and showing the returned cluster
#'   assignment.
#' @name equivalence
#' @inheritParams is
#' @param method Character string indicating which method
#'   should be used to choose the number of clusters to cut
#'   the tree at.
#'   By default "strict" to return classes with members only
#'   when strictly equivalent.
#'   Other options to relax this strict assumption generally
#'   provide more useful results and include "elbow".
#' @importFrom stats as.dist hclust cutree coef
#' @importFrom sna gcor
#' @references 
#'  Thorndike, Robert L. 1953. 
#'  "Who Belongs in the Family?". 
#'  _Psychometrika_.
#'  18 (4): 267–276. 
#'  \doi{10.1007/BF02289263}
NULL

#' @describeIn equivalence Returns nodes' membership in 
#'   structurally equivalent classes
#' @examples
#' (nse <- node_structural_equivalence(mpn_elite_mex, "elbow"))
#' plot(nse)
#' (nse2 <- node_structural_equivalence(mpn_elite_usa_advice, "elbow"))
#' plot(nse2)
#' @export
node_structural_equivalence <- function(object, 
                                        method = c("strict", "elbow", "silhouette")){
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
  if(method == "elbow") k <- k_elbow(hc, object, mat)
  if(method == "silhouette") k <- k_silhouette(hc, object, distances)
  out <- make_partition(cutree(hc, k), object)
  attr(out, "hc") <- hc
  attr(out, "k") <- k
  out
}

#' @describeIn equivalence Returns nodes' membership in 
#'   regularly equivalent classes
#' @examples
#' (nre <- node_regular_equivalence(mpn_elite_mex, "elbow"))
#' plot(nre)
#' (nre2 <- node_regular_equivalence(mpn_elite_usa_advice, "elbow"))
#' plot(nre2)
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
  if(method == "elbow") k <- k_elbow(hc, object, triads)
  out <- make_partition(cutree(hc, k), object)
  attr(out, "hc") <- hc
  attr(out, "k") <- k
  out
}

#' @describeIn equivalence Returns nodes' membership in 
#'   automorphically equivalent classes
#' @examples 
#' (nae <- node_regular_equivalence(mpn_elite_mex, "elbow"))
#' plot(nae)
#' (nae2 <- node_regular_equivalence(mpn_elite_usa_advice, "elbow"))
#' plot(nae2)
#' @export
node_automorphic_equivalence <- function(object,
                                         method = c("strict", "elbow")){
  paths <- node_path_census(object)
  correlations <- cor(t(paths))
  dissimilarity <- 1 - correlations
  distances <- stats::as.dist(dissimilarity)
  hc <- stats::hclust(distances)
  
  method <- match.arg(method)
  if(method == "strict") k <- k_strict(hc, object)
  if(method == "elbow") k <- k_elbow(hc, object, paths)
  out <- make_partition(cutree(hc, k), object)
  attr(out, "hc") <- hc
  attr(out, "k") <- k
  out
}

k_strict <- function(hc, object){
  zero_merged <- hc$merge[hc$height == 0,]
  k <- graph_nodes(object) - sum(zero_merged < 0) + sum(zero_merged > 0)
}

k_elbow <- function(hc, object, census){
  
  vertices <- graph_nodes(object)
  observedcorrelation <- cor(t(census))

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
  # dafr$correct <- ifelse(dafr$clusters == elbow_finder(dafr$clusters, 
  #                                                        dafr$correlations),
  #                          "#E20020", "#6f7072")
  elbow_finder(dafr$clusters, dafr$correlations)
}

clusterCorr <- function(observed_cor_matrix, cluster_vector) {
  num_vertices = nrow(observed_cor_matrix)
  cluster_cor_mat <- observed_cor_matrix
  
  obycor <- function(i, j) 
    mean(observed_cor_matrix[which(cluster_vector[row(observed_cor_matrix)] ==
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

elbow_finder <- function(x_values, y_values) {
  # Max values to create line
  if(min(x_values)==1) x_values <- x_values[2:length(x_values)]
  if(min(y_values)==0) y_values <- y_values[2:length(y_values)]
  max_df <- data.frame(x = c(min(x_values), max(x_values)), 
                       y = c(min(y_values), max(y_values)))
  # Creating straight line between the max values
  fit <- stats::lm(max_df$y ~ max_df$x)
  # Distance from point to line
  distances <- vector()
  for (i in seq_len(length(x_values))) {
    distances <- c(distances,
                   abs(stats::coef(fit)[2]*x_values[i] -
                         y_values[i] +
                         coef(fit)[1]) /
                     sqrt(stats::coef(fit)[2]^2 + 1^2))
  }
  # Max distance point
  x_max_dist <- x_values[which.max(distances)]
  x_max_dist
}
