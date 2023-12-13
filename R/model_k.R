#' Methods for selecting clusters
#' 
#' These functions are generally not user-facing but used internally
#' in e.g. the `*_equivalence()` functions.
#' They are documented here.
#' @inheritParams cohesion
#' @param hc A hierarchical clustering object.
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
#' @name kselect
NULL

#' @describeIn kselect Selects a number of clusters in which there is no
#'   distance between cluster members.
#' @export
k_strict <- function(hc, .data){
  zero_merged <- hc$merge[round(hc$height,4) == 0,]
  k <- nrow(zero_merged) + manynet::network_nodes(.data) - sum(zero_merged < 0) + sum(zero_merged > 0)
  k
}

#' @describeIn kselect Selects a number of clusters in which there is 
#'   a fair trade-off between parsimony and fit according to the elbow method.
#' @param census A motif census object.
#' @param range An integer indicating the maximum number of options to consider.
#'   The minimum of this and the number of nodes in the network is used. 
#' @export
k_elbow <- function(hc, .data, census, range){
  
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
  
  vertices <- manynet::network_nodes(.data)
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

#' @describeIn kselect Selects a number of clusters that
#'   optimises the silhouette score.
#' @export
k_silhouette <- function(hc, .data, range){
  kcs <- 2:min(range, manynet::network_nodes(.data))
  ns <- seq_len(manynet::network_nodes(.data))
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
