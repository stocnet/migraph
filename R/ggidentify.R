#' Visualising graphs and identifying nodes with maximum values of the specified
#' measure.
#' @param object a migraph-consistent object
#' @param FUN some arbitrary function that runs on the object and
#' returns a numeric vector that can be used to scale the nodes
#' @examples 
#' ggidentify(brandes, node_degree)
#' ggidentify(brandes, node_betweenness)
#' ggidentify(brandes, node_closeness)
#' ggidentify(brandes, node_eigenvector)
#' @export
ggidentify <- function(object, FUN){

  measure <- FUN(object)
  colord <- ifelse(measure == max(measure),
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
