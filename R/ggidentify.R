#' Visualising graphs and identifying nodes with maximum values of the specified
#' measure.
#' @param object a migraph-consistent object
#' @param node_measure some arbitrary function that runs on the object and
#' returns a numeric vector that can be used to scale the nodes
#' @param identify_function a function for the identification of a single node,
#' e.g. max, min, mean, etc.
#' @name ggtools
NULL

#' @rdname ggtools
#' @examples
#' ggidentify(brandes, node_degree)
#' ggidentify(brandes, node_betweenness)
#' ggidentify(brandes, node_closeness)
#' ggidentify(brandes, node_eigenvector)
#' @export
ggidentify <- function(object, node_measure, identify_function = max) {
  object <- as_tidygraph(object)
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

#' @rdname ggtools
#' @examples
#' ggdistrib(brandes, node_degree)
#' ggdistrib(brandes, node_betweenness)
#' ggdistrib(brandes, node_closeness)
#' ggdistrib(brandes, node_eigenvector)
#' @export
ggdistrib <- function(object, node_measure){
  distrib <- node_measure(object)
  ggplot2::ggplot(as.data.frame(distrib)) +
    ggplot2::geom_histogram(ggplot2::aes(distrib),
                            binwidth = ifelse(max(distrib) > 1, 1,
                                              ifelse(max(distrib) > .1,
                                                     .1,
                                                     .01))) +
    ggplot2::theme_classic() +
    ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = "grey90")) +
    ggplot2::xlab("Score") +
    ggplot2::ylab("Frequency")
}
