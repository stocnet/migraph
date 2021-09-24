#' Plot the evolution of a network
#' 
#' This function offers a method to plot a network 
#' at two or more timepoints for quick and easy comparison.
#' The function is currently limited to two networks
#' and only the layout given by the first or last network,
#' but further extensions expected. 
#' @param ... two or more networks
#' @param layout an igraph layout. Default is Kamada-Kawai ("kk")
#' @param based_on whether the layout of the joint plots should
#' be based on the "first" or the "last" network.
#' @importFrom gridExtra grid.arrange
#' @examples 
#' mpn_elite_mex2 <- mpn_elite_mex  %>%
#'                   tidygraph::activate(edges) %>%
#' tidygraph::reroute(from = sample.int(11, 44, replace = TRUE), 
#' to = sample.int(11, 44, replace = TRUE))
#' ggevolution(mpn_elite_mex, mpn_elite_mex2)
#' ggevolution(mpn_elite_mex, mpn_elite_mex2, based_on = "last")
#' ggevolution(mpn_elite_mex, mpn_elite_mex2, based_on = "both")
#' @export
ggevolution <- function(..., layout = "kk", 
                        based_on = c("first", "last", "both")){
  
  index <- nodes <- name <- NULL # to avoid CMD check notes
  
  networks <- list(...)
  networks <- lapply(networks, as_tidygraph)
  if(length(networks)!=2) stop("This function currently only accepts two networks at a time.")
  
  l1 <- ggraph::create_layout(networks[[1]], layout = "igraph", algorithm = layout)
  l2 <- ggraph::create_layout(networks[[2]], layout = "igraph", algorithm = layout)
  
  based_on <- match.arg(based_on)
  if(based_on == "first"){
    l2$x <- l1$x
    l2$y <- l1$y
  } else if (based_on == "last"){
    l1$x <- l2$x
    l1$y <- l2$y
  } else if (based_on == "both"){
    l3 <- as_igraph(networks[[1]]) + as_igraph(networks[[2]])
    l3 <- ggraph::create_layout(l3, layout = "igraph", algorithm = layout)
    l1$x <- l2$x <- l3$x
    l1$y <- l2$y <- l3$y
  } else warning("No other bases currently implemented. Defaulting to individual layouts.")
  
  g1 <- ggraph::ggraph(l1) +
    ggraph::geom_node_point() + 
    ggraph::geom_edge_link(ggplot2::aes(alpha = stat(index)), show.legend = FALSE) +
    ggplot2::theme_void() + ggraph::geom_node_text(ggplot2::aes(label = name))
  g2 <- ggraph::ggraph(l2) +
    ggraph::geom_node_point() + 
    ggraph::geom_edge_link(ggplot2::aes(alpha = stat(index)), show.legend = FALSE) +
    ggplot2::theme_void() + ggraph::geom_node_text(ggplot2::aes(label = name))
  
  gridExtra::grid.arrange(g1, g2, ncol = length(networks))
  
}