#' ggdistrib

#' `ggdistrib()` plots the distribution of a node level measure as a simple
#' histogram.
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
  measure <- as.character(substitute(node_measure))
  ggplot2::ggplot(as.data.frame(distrib)) +
    ggplot2::geom_histogram(ggplot2::aes(distrib),
                            binwidth = ifelse(max(distrib) > 1, 1,
                                              ifelse(max(distrib) > .1,
                                                     .1,
                                                     .01))) +
    ggplot2::ggtitle(paste0("Distribution of ",
                            measure)) +
    ggplot2::theme_classic() +
    ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = "grey90")) +
    ggplot2::xlab("Score") +
    ggplot2::ylab("Frequency")
}
