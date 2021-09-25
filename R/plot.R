#' Plotting of one-mode and two-mode graphs
#' @param x A migraph-compatible object, especially an igraph graph object.
#' @param ... Additional arguments passed on to igraph.
#' @family plotting
#' @examples 
#' mat1 <- create_ring(5,10)
#' plot(mat1)
#' @export
plot.igraph <- function(x, ...){
  object <- as_igraph(x)
  if(is_bipartite(object)){
    igraph::V(object)$color <- ifelse(igraph::V(object)$type, "black", "grey")
    igraph::V(object)$shape <- ifelse(igraph::V(object)$type, "square", "circle")
    igraph::V(object)$label <- NA
    lo <- igraph::layout_as_bipartite(object)
    lo[,2] <- abs(lo[,2]-1)
    igraph::plot.igraph(object, layout = lo, ...)
  } else {
    igraph::V(object)$color <- "white"
    lo <- igraph::layout_nicely(object)
      if(nrow(lo)==2){
        lo[1,] <- c(0,0)
        lo[2,] <- c(1,0)
      } 
      if(nrow(lo)==3){
        lo[1,] <- c(0,0)
        lo[2,] <- c(1,0)
        lo[3,] <- c(.5,.866)
      } 
      if(nrow(lo)==4){
        lo[1,] <- c(0,0)
        lo[2,] <- c(1,0)
        lo[3,] <- c(0,1)
        lo[4,] <- c(1,1)
      } 
    igraph::plot.igraph(object, layout = lo, vertex.label.color = "black", ...)
  }
}

#' ggplot2-based plotting of blockmodel results
#' @param x A blockmodel-class object.
#' @param ... Additional arguments passed on to ggplot2.
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot geom_tile aes scale_fill_gradient theme_grey labs theme scale_x_discrete scale_y_discrete geom_vline geom_hline element_blank element_text
#' @importFrom rlang .data
#' @examples 
#' usa_concor <- blockmodel_concor(mpn_elite_usa_advice)
#' plot(usa_concor)
#' @export
plot.blockmodel <- function(x, ...){
  
  plot_data <- x$blocked.data
  plot_data <- as.data.frame(plot_data) %>%
    tibble::rownames_to_column("Var1") %>%
    tidyr::pivot_longer(!.data$Var1, names_to = "Var2", values_to = "value")
  
  g <- ggplot2::ggplot(plot_data, ggplot2::aes(.data$Var2, .data$Var1)) + 
    ggplot2::geom_tile(ggplot2::aes(fill = .data$value), colour = "white") + 
    ggplot2::scale_fill_gradient(low = "white", high = "black") +
    ggplot2::theme_grey(base_size = 9) + 
    ggplot2::labs(x = "", y = "") + 
    ggplot2::theme(legend.position = "none",
                   axis.ticks = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(size = 9 * 0.8, colour = "grey50"),
                   axis.text.x = ggplot2::element_text(size = 9 * 0.8, 
                                                       angle = 30, hjust = 0, colour = "grey50"))
  
  if(x$modes==1){
    g <- g + ggplot2::scale_x_discrete(expand = c(0, 0), position = "top", limits = colnames(x$blocked.data)[x$order.vector]) +
      ggplot2::scale_y_discrete(expand = c(0, 0), limits = rev(rownames(x$blocked.data)[x$order.vector])) + 
      ggplot2::geom_vline(xintercept = c(1+which(diff(x$block.membership)!=0))-.5, colour = "red") +
      ggplot2::geom_hline(yintercept = nrow(x$blocked.data) - c(1+which(diff(x$block.membership)!=0))+1.5, colour = "red")
  } else {
    g <- g + ggplot2::scale_y_discrete(expand = c(0, 0), limits = rev(rownames(x$blocked.data)[x$order.vector$nodes1])) +
      ggplot2::scale_x_discrete(expand = c(0, 0), position = "top", limits = colnames(x$blocked.data)[x$order.vector$nodes2]) + 
      ggplot2::geom_vline(xintercept = c(1+which(diff(x$block.membership$nodes2)!=0))-.5, colour = "blue") +
      ggplot2::geom_hline(yintercept = nrow(x$blocked.data) - c(1+which(diff(x$block.membership$nodes1)!=0))+1.5, colour = "red")
  }
  g
  
}
