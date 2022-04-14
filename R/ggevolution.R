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
#' @importFrom rlang .data
#' @importFrom ggplot2 stat
#' @importFrom patchwork plot_layout
#' @examples
#' mpn_elite_mex <- mpn_elite_mex %>% to_subgraph(in_mpn == 1)
#' mpn_elite_mex2 <- mpn_elite_mex  %>%
#'                   tidygraph::activate(edges) %>%
#' tidygraph::reroute(from = sample.int(11, 44, replace = TRUE),
#' to = sample.int(11, 44, replace = TRUE))
#' ggevolution(mpn_elite_mex, mpn_elite_mex2)
#' ggevolution(mpn_elite_mex, mpn_elite_mex2, based_on = "last")
#' ggevolution(mpn_elite_mex, mpn_elite_mex2, based_on = "both")
#' @export
ggevolution <- function(..., layout = "kk", 
                        based_on = c("first", "last", "both")) {
  networks <- list(...)
  networks <- lapply(networks, as_tidygraph)
  if (length(networks) != 2) {
    stop("This function currently only accepts two networks at a time.")
  }
  l1 <- ggraph::create_layout(networks[[1]],
                              layout = "igraph",
                              algorithm = layout)
  l2 <- ggraph::create_layout(networks[[2]],
                              layout = "igraph",
                              algorithm = layout)
  
  based_on <- match.arg(based_on)
  if (based_on == "first") {
    l2$x <- l1$x
    l2$y <- l1$y
  } else if (based_on == "last") {
    l1$x <- l2$x
    l1$y <- l2$y
  } else if (based_on == "both") {
    l3 <- igraph::union(as_igraph(networks[[1]]), as_igraph(networks[[2]]))
    l3 <- ggraph::create_layout(l3, layout = "igraph", algorithm = layout)
    l1$x <- l2$x <- l3$x
    l1$y <- l2$y <- l3$y
  }
  g1 <- ggraph::ggraph(l1) +
    ggraph::geom_node_point() +
    ggraph::geom_edge_link(alpha = 0.4) +
    ggplot2::theme_void() +
    ggraph::geom_node_text(ggplot2::aes(label = .data$name), 
                           repel = TRUE)
  g2 <- ggraph::ggraph(l2) +
    ggraph::geom_node_point() +
    ggraph::geom_edge_link(alpha = 0.4) +
    ggplot2::theme_void() +
    ggraph::geom_node_text(ggplot2::aes(label = .data$name), 
                           repel = TRUE)
  g1 + g2
}

#' Plotting a network at a particular timepoint (year)
#' 
#' @param edgelist A manyverse edgelist, expecting `Beg` and `End` variables,
#' among others
#' @param year Numeric year, gets expanded to first of January that year
#' @param ... Additional arguments passed on to `autographr()`.
#' @return A plot of the network of agreements signed in the specified year.
#' @importFrom ggraph geom_edge_link geom_node_point geom_node_text
#' @importFrom ggplot2 aes theme_void
#' @examples 
#' \dontrun{
#' ggatyear(membs, 1900)
#' }
#' @export
ggatyear <- function(edgelist, year, ...) {
  name <- type <- NULL # Initialize variables
  # Some input checks and corrections
  if (!(is.numeric(year))) year <- as.numeric(year)
  if (!("Beg" %in% names(edgelist))) stop("Your edgelist does not contain a date column named Beg.")
  if (!("End" %in% names(edgelist))) stop("Your edgelist does not contain a date column named End.")
  # Create subsetted graph
  graph <- as_tidygraph(to_subgraph(edgelist, .data$Beg >
                                        paste0(year, "-01-01") &
                                        .data$Beg < paste0(year + 1, "-01-01")))
  # Plot graph with autographr
  autographr(graph, ...) +
    ggtitle(year)
}
