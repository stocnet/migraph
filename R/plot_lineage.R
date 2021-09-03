#' Plot lineage graph
#'
#' Lineage implies a direct descent from an ancestor; ancestry or pedigree.
#' That is, how observation derives and is connected to previous observations. 
#' The function plots a lineage graph of citations, amendments, and more, for example.
#' @param x A migraph-consistent network/graph
#' @importFrom ggraph create_layout ggraph geom_edge_diagonal
#' @importFrom ggplot2 theme_void coord_flip
#' @examples
#' \donttest{
#' cites <- tibble::tibble(qID1 = c("BNLHPB_2016P:BNLHPB_1970A",
#' "PARIS_2015A","INOOTO_2015A", "RUS-USA[UUF]_2015A",
#' "RUS-USA[UUF]_2015A", "RUS-USA[UUF]_2015A", "RUS-USA[UUF]_2015A",
#' "INECHA_2015O", "ST04DC_2014P", "ST04DC_2014P"),
#' qID2 = c("BNLHPB_1977P:BNLHPB_1970A", "UNFCCC_1992A", "INOOTO_2005A",
#' "RUS-USA[MFR]_1988A", "PS07UF_2009A", "UNCLOS_1982A", "UNCLOS_1982A",
#' "ERECHA_1991O", "AI07EM_1998A", "CNEWNH_1979A"))
#' plot_lineage(cites)
#' }
#' @export
plot_lineage <- function(x){
  index <- NULL # to avoid CMD check notes
  x <- as_tidygraph(x)
  lo <- ggraph::create_layout(x, layout = "igraph", algorithm = "sugiyama", maxiter = 100000)
  ggraph::ggraph(x, graph = lo) +
    ggraph::geom_edge_diagonal(aes(alpha = stat(index)), show.legend = FALSE) +
    ggplot2::theme_void() + ggplot2::coord_flip()
}
