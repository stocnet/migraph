#' ggidentify
#' 
#' @description
#' This function is deprecated and its functionality is included in the more
#' general purpose `autographr()` function. Please refer to its documentation
#' for more details about the new implementation.
#' 
#' Visualising graphs and identifying nodes with maximum values of the specified
#' measure.
#' @param object a migraph-consistent object
#' @param node_measure some arbitrary function that runs on the object and
#' returns a numeric vector that can be used to scale the nodes
#' @param identify_function a function for the identification of a single node,
#' e.g. max, min, mean, etc.
#' @keywords internal
#' @examples
#' ggidentify(brandes, node_degree)
#' # ->
#' autographr(brandes, node_measure = node_degree, identify_function = max)
#' ggidentify(brandes, node_betweenness)
#' # ->
#' autographr(brandes, node_measure = node_betweenness, identify_function = max)
#' ggidentify(brandes, node_closeness)
#' # ->
#' autographr(brandes, node_measure = node_closeness, identify_function = max)
#' ggidentify(brandes, node_eigenvector)
#' # ->
#' autographr(brandes, node_measure = node_eigenvector, identify_function = max)
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

#' Visualising graphs and identifying nodes with maximum values of the specified
#' measure.
#' @param object a migraph-consistent object
#' @param node_measure some arbitrary function that runs on the object and
#' returns a numeric vector that can be used to scale the nodes
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
