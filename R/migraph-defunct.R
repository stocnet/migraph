#' Functions that have been renamed, superseded, or are no longer working
#' 
#' `r lifecycle::badge("deprecated")`
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
  .Deprecated("edge_reciprocal", package = "migraph",
              old = "edge_mutual")
  edge_reciprocal(object = object)
}

#' @describeIn defunct Deprecated on 2022-05-27.
#' @export
ggtree <- function(hc, k = NULL){
  .Deprecated("plot.partition", package = "migraph",
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

#' @describeIn defunct Deprecated on 2022-05-27.
#' @export
ggidentify_clusters <- function(hc, census, method = c("elbow", "strict")){
  .Deprecated("plot.partition", package = "migraph",
              old = "ggidentify_clusters")
  
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
  
  # plotting
  ggplot2::ggplot(dafr, aes(x = clusters, y = correlations)) +
    ggplot2::geom_line(color = "#6f7072") +
    ggplot2::geom_point(aes(color = correct), size = 2) +
    ggplot2::scale_color_manual(values = c("#6f7072", "#E20020")) +
    # ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::theme_minimal() +
    ggplot2::guides(color = "none")
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


