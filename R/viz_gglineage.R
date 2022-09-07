#' Plot lineage graph
#'
#' Lineage implies a direct descent from an ancestor; ancestry or pedigree.
#' That is, how observation derives and is connected to previous observations. 
#' The function plots a lineage graph of citations, amendments, and more, for example.
#' @param object A migraph-consistent network/graph.
#' @param labels Whether to plot node labels or not. Default: TRUE.
#' @importFrom ggraph create_layout ggraph geom_edge_diagonal
#' @importFrom ggplot2 theme_void coord_flip scale_x_reverse
#' @importFrom rlang .data
#' @examples
#' cites <- dplyr::tibble(qID1 = c("BNLHPB_2016P:BNLHPB_1970A",
#' "PARIS_2015A","INOOTO_2015A", "RUS-USA[UUF]_2015A",
#' "RUS-USA[UUF]_2015A", "RUS-USA[UUF]_2015A", "RUS-USA[UUF]_2015A",
#' "INECHA_2015O", "ST04DC_2014P", "ST04DC_2014P"),
#' qID2 = c("BNLHPB_1977P:BNLHPB_1970A", "UNFCCC_1992A", "INOOTO_2005A",
#' "RUS-USA[MFR]_1988A", "PS07UF_2009A", "UNCLOS_1982A", "UNCLOS_1982A",
#' "ERECHA_1991O", "AI07EM_1998A", "CNEWNH_1979A"))
#' gglineage(cites)
#' @export
gglineage <- function(object, labels = TRUE){
  nodes <- NULL # Avoid R CMD check note
  object <- as_tidygraph(object)
  if (all(grepl("\\d{4}", node_names(object)))) {
    object <- object %>%
      activate(nodes) %>%
      mutate(year = as.numeric(sub(".*_([0-9]{4}).*", "\\1", node_names(object))))
  }
  lo <- ggraph::create_layout(object, layout = "alluvial")
  if (!is.null(lo$year)) lo$x = lo$year else 
    if ("year" %in% graph_node_attributes(object)) lo$x = node_attribute(object, "year")
  if (is.null(lo$name)) lo$name = node_names(object)
  g <- ggraph::ggraph(object, graph = lo) +
    ggraph::geom_edge_diagonal(aes(edge_color = as.factor(.data$from)),
                               show.legend = FALSE) +
    ggraph::geom_node_point(shape = 3) +
    ggplot2::theme_void()
  if (labels) {
    g <- g + ggraph::geom_node_text(aes(label = .data$name),
                                    nudge_x = 0.1,
                                    repel = TRUE)
  }
  g
}
