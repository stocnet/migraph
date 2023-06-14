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
#' @family memberships
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
NULL

#' @describeIn equivalence Returns nodes' membership in 
#'   according to their equivalence with respective to some census/class
#' @export
node_equivalence <- function(.data, census,
                             k = c("silhouette", "elbow", "strict"),
                             cluster = c("hierarchical", "concor"),
                             distance = c("euclidean", "maximum", "manhattan", 
                                          "canberra", "binary", "minkowski"),
                             range = 8L){
  hc <- switch(match.arg(cluster),
               hierarchical = cluster_hierarchical(census, match.arg(distance)),
               concor = cluster_concor(.data, census))
  
  if(!is.numeric(k))
    k <- switch(match.arg(k),
                strict = k_strict(hc, .data),
                elbow = k_elbow(hc, .data, census, range),
                silhouette = k_silhouette(hc, .data, range))
  
  out <- make_node_member(stats::cutree(hc, k), .data)
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
node_structural_equivalence <- function(.data,
                                        k = c("silhouette", "elbow", "strict"),
                                        cluster = c("hierarchical", "concor"),
                                        distance = c("euclidean", "maximum", "manhattan", 
                                                     "canberra", "binary", "minkowski"),
                                        range = 8L){
  mat <- node_tie_census(.data)
  if(any(colSums(t(mat))==0)){
    mat <- cbind(mat, (colSums(t(mat))==0))
  } 
  node_equivalence(.data, mat, 
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
node_regular_equivalence <- function(.data, 
                                     k = c("silhouette", "elbow", "strict"),
                                     cluster = c("hierarchical", "concor"),
                                     distance = c("euclidean", "maximum", "manhattan", 
                                                  "canberra", "binary", "minkowski"),
                                     range = 8L){
  if(manynet::is_twomode(.data)){
    mat <- as.matrix(node_quad_census(.data))
  } else {
    mat <- node_triad_census(.data)
  }
  if(any(colSums(mat) == 0)) mat <- mat[,-which(colSums(mat) == 0)]
  node_equivalence(.data, mat, 
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
node_automorphic_equivalence <- function(.data,
                                         k = c("silhouette", "elbow", "strict"),
                                         cluster = c("hierarchical", "concor"),
                                         distance = c("euclidean", "maximum", "manhattan", 
                                                      "canberra", "binary", "minkowski"),
                                         range = 8L){
  mat <- node_path_census(.data)
  node_equivalence(.data, mat, 
                   k = k, cluster = cluster, distance = distance, range = range)
}
