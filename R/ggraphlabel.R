#' Plot graph with quick labels
#' 
#' For quick and easy graphing of networks with labels
#' @param x A migraph-consistent network/graph
#' @param algorithm An initial network layout,
#' currently either Kamada-Kawai ("kk") or
#' Fruchterman-Reingold ("fr")
#' @importFrom ggraph create_layout ggraph geom_edge_link geom_node_text geom_conn_bundle get_con geom_node_point
#' @importFrom ggplot2 theme_void
#' @importFrom igraph as_edgelist
#' @importFrom stats dist
#' @examples
#' ggraphlabel(adolescent_society)
#' @export
ggraphlabel <- function(x, algorithm = c("kk","fr")){
  name <- NULL # initialize variables to avoid CMD check notes
  x <- as_tidygraph(x)
  algorithm <- match.arg(algorithm)

  gg <- ggraph::create_layout(x, layout = "igraph", 
                              algorithm = algorithm, maxiter = 10000)
  
  ggraph::ggraph(x, graph = gg) +
    ggraph::geom_conn_bundle(data = ggraph::get_con(from = igraph::as_edgelist(x, names = FALSE)[,1], 
                                                    to = igraph::as_edgelist(x, names = FALSE)[,2]), alpha = 0.1) +
    # ggraph::geom_edge_link(arrow = arrow(length = unit(3, 'mm')), 
    #                        start_cap = circle(4, 'mm'),
    #                        end_cap = circle(4, 'mm'), show.legend = FALSE) +
    ggraph::geom_node_text(aes(label = name)) + ggplot2::theme_void()
}