#' Visualising graphs and identifying nodes with maximum values of the specified
#' measure.
#' @param object a migraph-consistent object
#' @param node_measure some arbitrary function that runs on the object and
#' returns a numeric vector that can be used to scale the nodes
#' @param identify_function a function for the identification of a single node,
#' e.g. max, min, mean, etc.
#' @examples 
#' ggidentify(brandes, node_degree)
#' ggidentify(brandes, node_betweenness)
#' ggidentify(brandes, node_closeness)
#' ggidentify(brandes, node_eigenvector)
#' @export
ggidentify <- function(object, node_measure, identify_function = max){

  measure <- node_measure(object)
  colord <- ifelse(measure == identify_function(measure),
               "max", "other")
  
  ggraph::ggraph(object) + 
    ggplot2::theme_void() +
    ggraph::geom_edge_link() +
    ggraph::geom_node_point(aes(size = measure,
                                colour = colord)) +
    ggplot2::scale_color_manual(breaks = c("max", "other"),
                                values = c("red", "blue")) + 
    ggplot2::theme(legend.position = "none")
}

#' Plots for deciding on the number of network clusters
#' @param hc a hierarchical cluster object
#' @param k number of clusters. By default NULL,
#' but, if specified, `ggtree` will color branches and
#' add a line to indicate where the corresponding cluster
#' cut would be.
#' @param mat the matrix
#' @param method only "elbow" is currently implemented.
#' @name identify_clusters
#' @importFrom ggdendro ggdendrogram
#' @importFrom RColorBrewer brewer.pal
#' @importFrom stats cutree
#' @examples
#' res <- cluster_regular_equivalence(mpn_elite_mex)
#' ggtree(res, 4)
#' @export
ggtree <- function(hc, k = NULL){
  
  if(is.null(k)){
    ggdendro::ggdendrogram(hc, rotate = TRUE)
  } else {
    
    colors <- RColorBrewer::brewer.pal(k, "Set3")
    colors <- (colors[stats::cutree(hc, k = k)])[hc$order]
    
    ggdendro::ggdendrogram(hc, rotate = TRUE) +
      ggplot2::geom_hline(yintercept = hc$height[length(hc$order) - k], linetype = 2,
                          color = "#E20020") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(colour = "#E20020"),
                     axis.text.y = suppressWarnings(ggplot2::element_text(colour = colors)))
  }
  
}

