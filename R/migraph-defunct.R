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

#' @describeIn defunct Deprecated on 2022-01-24.
#' This function is deprecated and its functionality is included in the more
#' general purpose `autographr()` function. Please refer to its documentation
#' for more details about the new implementation.
#' @export
ggidentify <- function(object, node_measure, identify_function = max) {
  # Deprecating the function for the time being. --> defunct in the next minor?
  .Deprecated("autographr", package = "migraph",
              msg = paste("This function has been included in the",
                          "`autographr()` function. Please run",
                          "`autographr(object, node_measure,",
                          "identify_function)` instead.",
                          sep = " "),
              old = "ggidentify")
  # The function
  object <- as_tidygraph(object)
  measure <- node_measure(object)
  colord <- ifelse(measure == identify_function(measure),
                   "max", "other")
  # Generate output
  ggraph::ggraph(object) +
    ggplot2::theme_void() +
    ggraph::geom_edge_link() +
    ggraph::geom_node_point(aes(size = measure,
                                colour = colord)) +
    ggplot2::scale_color_manual(breaks = c("max", "other"),
                                values = c("red", "blue")) +
    ggplot2::theme(legend.position = "none")
}

#' @describeIn defunct Deprecated on 2022-03-23.
#' This function is deprecated and its functionality is included in the more
#' general purpose `autographr()` function. Please refer to its documentation
#' for more details about the new implementation.
#' @export
ggdistrib <- function(object, node_measure){
  .Deprecated("plot.measure", package = "migraph",
              msg = paste("This function has been converted into a",
                          "`plot()` method for a 'measure' class object.", 
                          "Please pass an object resulting from a `node_()`",
                          "function to `plot()` to achieve the same result.",
                          sep = " "),
              old = "ggdistrib")
}

#' @describeIn defunct Deprecated on 2022-03-23.
#' @export
project_rows <- function(object){
  .Deprecated("to_mode1", package = "migraph",
              old = "project_rows")
  to_mode1(object)
}

#' @describeIn defunct Deprecated on 2022-03-23.
#' @export
project_cols <- function(object){
  .Deprecated("to_mode2", package = "migraph",
              old = "project_cols")
  to_mode2(object)
}

#' @describeIn defunct Deprecated on 2022-03-29.
#' @export
mutate_edges <- function(object, object2, attr_name){
  .Deprecated("join_edges", package = "migraph",
              old = "mutate_edges")
  join_edges(object = object, object2 = object2, 
             attr_name = attr_name)
}

#' @describeIn defunct Deprecated on 2022-04-05.
#' @export
edge_mutual <- function(object){
  .Deprecated("tie_is_reciprocated", package = "migraph",
              old = "edge_mutual")
  tie_is_reciprocated(object = object)
}

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
