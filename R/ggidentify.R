#' Visualising graphs and identifying nodes with maximum values
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
    ggraph::theme_graph() +
    ggraph::geom_edge_link() +
    ggraph::geom_node_point(aes(size = measure*4,
                                colour = colord)) +
    ggplot2::scale_color_manual(breaks = c("max", "other"),
                                values=c("red", "blue")) + 
    ggplot2::theme(legend.position = "none")
}