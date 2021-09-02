#' Plot lineage graph
#' 
#' Plot a lineage graph of citations, amendments, etc.
#' @param x A migraph-consistent network/graph
#' @importFrom ggraph create_layout ggraph geom_edge_diagonal
#' @importFrom ggplot2 theme_void coord_flip
#' @examples
#' \donttest{
#' cites <- qEnviron::references$ECOLEX_REF %>% 
#'   dplyr::filter(RefType == "Cites") %>%
#'   dplyr::select(qID1, qID2)
#' plot_lineage(cites)
#' }
#' @export
plot_lineage <- function(x){
  index <- NULL # initialize variables to avoid CMD check notes
  x <- as_tidygraph(x)
  lo <- ggraph::create_layout(x, layout = "igraph", algorithm = "sugiyama", maxiter = 100000)
  ggraph::ggraph(x, graph = lo) +
    ggraph::geom_edge_diagonal(aes(alpha = stat(index)), show.legend = FALSE) +
    ggplot2::theme_void() + ggplot2::coord_flip()
}
