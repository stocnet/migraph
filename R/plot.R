#' Plotting of one-mode and two-mode graphs
#' @param x A migraph-compatible object, especially an igraph graph object
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
    igraph::plot.igraph(object, ...)
  }
}
