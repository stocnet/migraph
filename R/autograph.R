#' Autograph with sensible defaults
#' @param object migraph-consistent object
#' @param node_color node variable in quotation marks
#' that should be used for colouring the nodes
#' @export
autograph <- function(object, node_color = NULL, ...){
  g <- as_tidygraph(object)
  
  # Add layout
  lo <- ggraph::create_layout(g, "fr")
  
  p <- ggraph::ggraph(lo) + ggraph::theme_graph()
  
  # Add edges
  if(is_directed(g)){
    p <- p + geom_edge_link(edge_alpha = 0.4,
                            arrow = arrow(angle = 15,
                                          length = unit(4, 'mm'),
                                          type = "closed"), 
                            end_cap = circle(3, 'mm'))
  } else {
    if(is_weighted(g)){
      p <- p + geom_edge_link0(aes(width = weight),
                               edge_alpha = 0.4) + 
        scale_edge_width_continuous(range = c(.2,1), 
                                    guide = "none")
    } else {
      p <- p + geom_edge_link0(edge_alpha = 0.4)
    }
  }
  
  # Add nodes
  if(is_twomode(g)){
    if(!is.null(node_color)){
      color_factor <- as.factor(get.vertex.attribute(g, node_color))
      p <- p + geom_node_point(aes(color = color_factor),
                               size = (100/igraph::vcount(g))/2,
                               shape = ifelse(igraph::V(g)$type, "square", "circle")) +
        scale_colour_brewer(palette = "Set1", guide = "none")
    } else {
      p <- p + geom_node_point(size = (100/igraph::vcount(g))/2,
                               shape = ifelse(igraph::V(g)$type, "square", "circle"))
    }
  } else {
    if(!is.null(node_color)){
      color_factor <- as.factor(get.vertex.attribute(g, node_color))
      p <- p + geom_node_point(aes(color = color_factor),
                               size = (100/igraph::vcount(g))/2) +
        scale_colour_brewer(palette = "Set1", guide = "none")
    } else {
      p <- p + geom_node_point(size = (100/igraph::vcount(g))/2)
    }
  }
  if(is_labelled(g)) p <- p + geom_node_label(aes(label = name),
                                              label.padding = 0.15,
                                              label.size = 0,
                                              repel = TRUE)
  
  p
}