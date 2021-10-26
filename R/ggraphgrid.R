#' Plot graph to grid
#'
#' For quick and easy graphing of networks to a grid plot
#' @details The function uses approximate pattern matching
#' to redistributes the coarse layouts on the square grid points, while
#' preserving the topological relationships among the nodes (see Inoue et al. 2012). 
#' @param x A migraph-consistent network/graph
#' @param algorithm An initial network layout,
#' currently either Kamada-Kawai ("kk") or
#' Fruchterman-Reingold ("fr")
#' @importFrom ggraph create_layout ggraph geom_edge_link geom_node_text
#' @importFrom ggraph geom_conn_bundle get_con geom_node_point
#' @importFrom ggplot2 theme_void
#' @importFrom igraph as_edgelist
#' @importFrom stats dist
#' @references
#' Inoue et al. (2012).
#' Application of Approximate Pattern Matching in Two Dimensional
#' Spaces to Grid Layout for Biochemical Network Maps.
#' PLoS One 7 (6): e37739. doi: https://doi.org/10.1371/journal.pone.0037739.
#' @export
ggraphgrid <- function(x, algorithm = c("kk", "fr")) {
  .Deprecated("autographr(x, 'frgrid'")
  if(algorithm == "fr") autographr(x, "frgrid")
  if(algorithm == "kk") autographr(x, "kkgrid")
}

