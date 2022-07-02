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
#' @family membership
#' @inheritParams is
#' @param census A matrix returned by a `node_*_census()` function.
#' @param k Typically a character string indicating which method
#'   should be used to select the number of clusters to return.
#'   By default `"silhouette"`, other options include `"elbow"` and `"strict"`.
#'   `"strict"` returns classes with members only when strictly equivalent.
#'   `"silhouette"` and `"elbow"` select classes based on the distance between
#'   clusters or between nodes within a cluster.
#'   Fewer, identifiable letters, e.g. `"e"` for elbow, is sufficient.
#'   Alternatively, if `k` is passed an integer, e.g. `k = 3`,
#'   then all selection routines are skipped in favour of this number of clusters.
#' @param cluster Character string indicating whether clusters should be 
#'   clustered hierarchically (`"hierarchical"`) or 
#'   through convergence of correlations (`"concor"`). 
#'   Fewer, identifiable letters, e.g. `"c"` for CONCOR, is sufficient.
#' @param distance Character string indicating which distance metric
#'   to pass on to `stats::dist`.
#'   By default `"euclidean"`, but other options include
#'   `"maximum"`, `"manhattan"`, `"canberra"`, `"binary"`, and `"minkowski"`.
#'   Fewer, identifiable letters, e.g. `"e"` for Euclidean, is sufficient.
#' @param range Integer indicating the maximum number of (k) clusters
#'   to evaluate.
#'   Ignored when `k = "strict"` or a discrete number is given for `k`.
#' @importFrom stats as.dist hclust cutree coef cor median
#' @importFrom sna gcor
#' @source \url{https://github.com/aslez/concoR}
#' @references 
#'  Thorndike, Robert L. 1953. 
#'    "Who Belongs in the Family?". 
#'    _Psychometrika_, 18(4): 267–76. 
#'    \doi{10.1007/BF02289263}.
#'
#' Rousseeuw, Peter J. 1987. 
#'   “Silhouettes: A Graphical Aid to the Interpretation and Validation of Cluster Analysis.” 
#'   _Journal of Computational and Applied Mathematics_, 20: 53–65. 
#'   \doi{10.1016/0377-0427(87)90125-7}.
NULL

#' @describeIn equivalence Returns nodes' membership in 
#'   according to their equivalence with respective to some census/class
#' @export
node_equivalence <- function(object, census,
                             k = c("silhouette", "elbow", "strict"),
                             cluster = c("hierarchical", "concor"),
                             distance = c("euclidean", "maximum", "manhattan", 
                                          "canberra", "binary", "minkowski"),
                             range = 8L){
  hc <- switch(match.arg(cluster),
               hierarchical = cluster_hierarchical(census, match.arg(distance)),
               concor = cluster_concor(object, census))
  
  if(!is.numeric(k))
    k <- switch(match.arg(k),
                strict = k_strict(hc, object),
                elbow = k_elbow(hc, object, census, range),
                silhouette = k_silhouette(hc, object, range))
  
  out <- make_node_member(stats::cutree(hc, k), object)
  attr(out, "hc") <- hc
  attr(out, "k") <- k
  out
}

#' @describeIn equivalence Returns nodes' membership in 
#'   structurally equivalent classes
#' @examples
#' \donttest{
#' (nse <- node_structural_equivalence(mpn_elite_usa_advice))
#' plot(nse)
#' }
#' @export
node_structural_equivalence <- function(object,
                                        k = c("silhouette", "elbow", "strict"),
                                        cluster = c("hierarchical", "concor"),
                                        distance = c("euclidean", "maximum", "manhattan", 
                                                     "canberra", "binary", "minkowski"),
                                        range = 8L){
  mat <- node_tie_census(object)
  if(any(colSums(t(mat))==0)){
    mat <- cbind(mat, (colSums(t(mat))==0))
  } 
  node_equivalence(object, mat, 
                   k = k, cluster = cluster, distance = distance, range = range)
}

#' @describeIn equivalence Returns nodes' membership in 
#'   regularly equivalent classes
#' @examples
#' \donttest{
#' (nre <- node_regular_equivalence(mpn_elite_usa_advice,
#'   cluster = "concor"))
#' plot(nre)
#' }
#' @export
node_regular_equivalence <- function(object, 
                                     k = c("silhouette", "elbow", "strict"),
                                     cluster = c("hierarchical", "concor"),
                                     distance = c("euclidean", "maximum", "manhattan", 
                                                  "canberra", "binary", "minkowski"),
                                     range = 8L){
  if(is_twomode(object)){
    mat <- as.matrix(node_quad_census(object))
  } else {
    mat <- node_triad_census(object)
  }
  if(any(colSums(mat) == 0)) mat <- mat[,-which(colSums(mat) == 0)]
  node_equivalence(object, mat, 
                   k = k, cluster = cluster, distance = distance, range = range)
}

#' @describeIn equivalence Returns nodes' membership in 
#'   automorphically equivalent classes
#' @examples
#' \donttest{
#' (nae <- node_automorphic_equivalence(mpn_elite_usa_advice,
#'   k = "elbow"))
#' plot(nae)
#' }
#' @export
node_automorphic_equivalence <- function(object,
                                         k = c("silhouette", "elbow", "strict"),
                                         cluster = c("hierarchical", "concor"),
                                         distance = c("euclidean", "maximum", "manhattan", 
                                                      "canberra", "binary", "minkowski"),
                                         range = 8L){
  mat <- node_path_census(object)
  node_equivalence(object, mat, 
                   k = k, cluster = cluster, distance = distance, range = range)
}

k_strict <- function(hc, object){
  zero_merged <- hc$merge[hc$height == 0,]
  k <- nrow(zero_merged) + graph_nodes(object) - sum(zero_merged < 0) + sum(zero_merged > 0)
  k
}

k_elbow <- function(hc, object, census, range){
  
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
  
  vertices <- graph_nodes(object)
  observedcorrelation <- cor(t(census))

  resultlist <- list()
  correlations <- vector()
  for (i in 2:min(range, vertices)) {
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
  dafr <- data.frame(clusters = 2:min(range, vertices), 
                     correlations = c(correlations))
  correct <- NULL # to satisfy the error god
  
  # k identification method
  elbow_finder(dafr$clusters, dafr$correlations)
}

k_silhouette <- function(hc, object, range){
  kcs <- 2:min(range, graph_nodes(object))
  ns <- seq_len(graph_nodes(object))
  distances <- hc$distances
  ks <- vector()
  for(kc in kcs){
    cand <- stats::cutree(hc, kc)
    ai <- vector()
    bi <- vector()
    for(i in ns){
      wig <- which(cand == cand[i])
      wig <- wig[wig != i]
      ai <- c(ai, 
              ifelse(length(wig)==0,
                     0, mean(as.matrix(distances)[i, wig])))
      wog <- which(cand != cand[i])
      bi <- c(bi, min(vapply(unique(cand[wog]), function(b){
        mean(as.matrix(distances)[i, wog[cand[wog]==b]])
      }, FUN.VALUE = numeric(1))))
    }
    si <- (bi - ai)/
      apply(data.frame(ai, bi), 1, max)
    ks <- c(ks, mean(si))
  }
  k <- which(ks == max(ks)) + 1
  k
}
