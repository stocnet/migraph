#' Functions that have been renamed, superseded, or are no longer working
#' 
#' `r lifecycle::badge("deprecated")`
#' Generally these functions have been superseded or renamed.
#' Upon using them, a message is provided directing the user to the new function.
#' However, at this stage of package development,
#' we generally clear older defunct functions at each minor release,
#' and so you are strongly encouraged to use the new functions/names/syntax
#' wherever possible and update your scripts accordingly.
#' @name defunct
#' @keywords internal
NULL

#' @describeIn defunct Deprecated on 2022-05-27.
#' @export
ggtree <- function(hc, k = NULL){
  .Deprecated("plot.member", package = "migraph",
              old = "ggtree")
  if (is.null(k)) {
    ggdendro::ggdendrogram(hc, rotate = TRUE)
  } else {
    colors <- colorsafe_palette[seq_len(k)]
    colors <- (colors[stats::cutree(hc, k = k)])[hc$order]
    ggdendro::ggdendrogram(hc, rotate = TRUE) +
      ggplot2::geom_hline(yintercept = hc$height[length(hc$order) - k],
                          linetype = 2,
                          color = "#E20020") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(colour = "#E20020"),
                     axis.text.y = suppressWarnings(
                       ggplot2::element_text(colour = colors)))
  }
  
}

#' @describeIn defunct Defunct on 2022-05-27.
#' @export
ggidentify_clusters <- function(hc, census, method = c("elbow", "strict")){
  .Defunct("plot.member", package = "migraph",
              msg = paste("This function has been superseded by",
                          "a different structure for blockmodelling.",
                          "The optimal `k` is now returned by the elbow method",
                          "internally, avoiding the need to do this visually."))
}

#' @describeIn defunct Deprecated on 2022-05-27.
#' @export
cluster_structural_equivalence <- function(object){
  .Deprecated("node_structural_equivalence", package = "migraph",
              msg = paste("Please use", 
                          "`node_structural_equivalence()`,",
                          "which will return a vector of partition assignments.",
                          sep = " "),
              old = "cluster_structural_equivalence")
}

#' @describeIn defunct Deprecated on 2022-05-27.
#' @export
cluster_regular_equivalence <- function(object){
  .Deprecated("node_regular_equivalence", package = "migraph",
              msg = paste("Please use", 
                          "`node_regular_equivalence()`,",
                          "which will return a vector of partition assignments.",
                          sep = " "),
              old = "cluster_regular_equivalence")
}

#' @describeIn defunct Deprecated on 2022-05-30.
#' @export
blockmodel_concor <- function(object, p = 1, 
                              cutoff = 0.999, max.iter = 25, 
                              block.content = "density"){
  .Deprecated("node_structural_equivalence", package = "migraph",
              msg = paste("This function has been converted into a",
                          "`cluster_concor()` clustering method for the", 
                          "`node_structural_equivalence()` function.",
                          "Please use that function to achieve the same result.",
                          sep = " "),
              old = "blockmodel_concor")
}

#' @describeIn defunct Deprecated on 2022-06-03.
#' @export
summarise_statistics <- function(node_measure, 
                                 clusters = NULL,
                                 sumFUN = mean){
  .Deprecated("summary", package = "migraph",
              old = "summarise_statistics")
  
  if (is.matrix(node_measure)) {
    out <- t(sapply(unique(clusters), 
                    function(x) apply(node_measure[clusters == x, ], 2, sumFUN)))
    rownames(out) <- unique(clusters)
  } else {
    out <- vapply(unique(clusters), 
                  function(x) sumFUN(node_measure[clusters == x]), FUN.VALUE = 1)
    names(out) <- unique(clusters)
  }
  out
}

#' @describeIn defunct Deprecated on 2022-06-03.
#' @export
blockmodel <- function(object, clusters){
  .Deprecated("to_blocks", package = "migraph",
              old = "blockmodel")
  
  to_blocks(object, membership = clusters)
}

#' @describeIn defunct Deprecated on 2022-06-03.
#' @export
print.block_model <- function(x, ...){
  .Deprecated("to_blocks", package = "migraph",
              msg = paste("This function is no longer necessary",
                          "with the new `to_blocks()`.",
                          sep = " "),
              old = "print.block_model")
}

#' @describeIn defunct Deprecated on 2022-06-03.
#' @export
reduce_graph <- function(blockmodel, block_labels = NULL){
  .Deprecated("to_blocks", package = "migraph",
              msg = paste("This function is no longer necessary",
                          "with the new `to_blocks()`.",
                          sep = " "),
              old = "print.blockmodel")
}  

#' @describeIn defunct Deprecated on 2022-06-08.
#' @export
ggatyear <- function(edgelist, year, ...) {
  .Deprecated("to_subgraph", package = "migraph",
              msg = paste("This function is no longer necessary.",
                          "Please use `autographr()` with `to_subgraph()`.",
                          sep = " "),
              old = "ggatyear")
  name <- type <- NULL # Initialize variables
  # Some input checks and corrections
  if (!(is.numeric(year))) year <- as.numeric(year)
  if (!("Beg" %in% names(edgelist))) stop("Your edgelist does not contain a date column named Beg.")
  if (!("End" %in% names(edgelist))) stop("Your edgelist does not contain a date column named End.")
  # Create subsetted graph
  graph <- as_tidygraph(to_subgraph(edgelist, .data$Beg >
                                      paste0(year, "-01-01") &
                                      .data$Beg < paste0(year + 1, "-01-01")))
  # Plot graph with autographr
  autographr(graph, ...) +
    ggtitle(year)
}

#' @describeIn defunct Deprecated on 2022-06-09.
#' @export
group_tie_census <- function(object, clusters, decimals = 2) {
  .Deprecated("summary.node_motif", package = "migraph",
              old = "group_tie_census")
  
  ties <- node_tie_census(object)
  cluster_tie_mat <- matrix(nrow = max(clusters), ncol = ncol(ties))
  for (i in seq_len(max(clusters))) {
    for (j in seq_len(ncol(ties))) {
      cluster_tie_mat[i, j] <- round(mean(ties[which(clusters == i), j]), decimals)
    }
  }
  colnames(cluster_tie_mat) <- colnames(ties)
  if(is.numeric(clusters)){
    rownames(cluster_tie_mat) <- paste("Block", 1:max(clusters))
  } else {
    rownames(cluster_tie_mat) <- clusters
  }
  cluster_tie_mat
}

#' @describeIn defunct Deprecated on 2022-06-09.
#' @export
group_triad_census <- function(object, clusters, decimals = 2) {

  .Deprecated("summary.node_motif", package = "migraph",
              old = "group_triad_census")
  
  triads <- node_triad_census(object)
  cluster_triad_mat <- matrix(nrow = max(clusters), ncol = ncol(triads))
  for (i in seq_len(max(clusters))) {
    for (j in seq_len(ncol(triads))) {
      cluster_triad_mat[i, j] <- round(mean(triads[which(clusters == i), j]), decimals)
    }
  }
  colnames(cluster_triad_mat) <- c("003", "012", "102", "021D",
                                   "021U", "021C", "111D", "111U",
                                   "030T", "030C", "201", "120D",
                                   "120U", "120C", "210", "300")
  if(is.numeric(clusters)){
    rownames(cluster_triad_mat) <- paste("Block", 1:max(clusters))
  } else {
    rownames(cluster_triad_mat) <- clusters
  }
  cluster_triad_mat 
}

#' @describeIn defunct Deprecated on 2022-06-28.
#' @export
add_edge_attribute <- function(object, attr_name, vector){
  .Deprecated("add_tie_attribute", package = "migraph",
              old = "add_edge_attribute")
  add_tie_attribute(object, attr_name, vector)
}

#' @describeIn defunct Deprecated on 2022-06-28.
#' @export
edge_attribute <- function(object, attribute){
  .Deprecated("tie_attribute", package = "migraph",
              old = "edge_attribute")
  tie_attribute(object, attribute)
}

#' @describeIn defunct Deprecated on 2022-06-28.
#' @export
edge_signs <- function(object){
  .Deprecated("tie_signs", package = "migraph",
              old = "edge_signs")
  tie_signs(object)
}

#' @describeIn defunct Deprecated on 2022-06-28.
#' @export
edge_weights <- function(object){
  .Deprecated("tie_weights", package = "migraph",
              old = "edge_weights")
  tie_weights(object)
}

#' @describeIn defunct Deprecated on 2022-06-28.
#' @export
edge_betweenness <- function(object, normalized = TRUE){
  .Deprecated("tie_betweenness", package = "migraph",
              old = "edge_betweenness")
  tie_betweenness(object, normalized)
}

#' @describeIn defunct Deprecated on 2022-06-28.
#' @export
edge_closeness <- function(object, normalized = TRUE){
  .Deprecated("tie_closeness", package = "migraph",
              old = "edge_closeness")
  tie_closeness(object, normalized)
}

#' @describeIn defunct Deprecated on 2022-06-28.
#' @export
edge_degree <- function(object, normalized = TRUE){
  .Deprecated("tie_degree", package = "migraph",
              old = "edge_degree")
  tie_degree(object, normalized)
}

#' @describeIn defunct Deprecated on 2022-06-28.
#' @export
edge_eigenvector <- function(object, normalized = TRUE){
  .Deprecated("tie_eigenvector", package = "migraph",
              old = "edge_eigenvector")
  tie_eigenvector(object, normalized)
}

#' @describeIn defunct Deprecated on 2022-06-28.
#' @export
edge_loop <- function(object){
  .Deprecated("tie_is_loop", package = "migraph",
              old = "edge_loop")
  tie_is_loop(object)
}

#' @describeIn defunct Deprecated on 2022-06-28.
#' @export
edge_multiple <- function(object){
  .Deprecated("tie_is_multiple", package = "migraph",
              old = "edge_multiple")
  tie_is_multiple(object)
}

#' @describeIn defunct Deprecated on 2022-06-28.
#' @export
edge_bridges <- function(object){
  .Deprecated("tie_is_bridge", package = "migraph",
              old = "edge_bridges")
  tie_is_bridge(object)
}

#' @describeIn defunct Deprecated on 2022-06-28.
#' @export
edge_reciprocal <- function(object){
  .Deprecated("tie_is_reciprocated", package = "migraph",
              old = "edge_reciprocal")
  tie_is_reciprocated(object)
}

#' @describeIn defunct Deprecated on 2022-06-28.
#' @export
graph_edge_attributes <- function(object){
  .Deprecated("graph_tie_attributes", package = "migraph",
              old = "graph_edge_attributes")
  graph_tie_attributes(object)
}

#' @describeIn defunct Deprecated on 2022-06-28.
#' @export
graph_edges <- function(object){
  .Deprecated("graph_ties", package = "migraph",
              old = "graph_edges")
  graph_ties(object)
}

#' @describeIn defunct Deprecated on 2022-06-30.
#' @export
node_cuts <- function(object){
  .Deprecated("node_is_cutpoint", package = "migraph",
              old = "node_cuts")
  node_is_cutpoint(object)
}

#' @describeIn defunct Deprecated on 2022-06-28.
#' @export
to_edges <- function(object){
  .Deprecated("to_ties", package = "migraph",
              old = "to_edges")
  to_ties(object)
}

#' @describeIn defunct Deprecated on 2022-07-03.
#' @export
join_edges <- function(object, object2, attr_name){
  .Deprecated("join_ties", package = "migraph",
              old = "join_edges")
  join_ties(object, object2, attr_name)
}

