#' Quickly graph networks with sensible defaults
#' 
#' The aim of this function is to provide users with a quick and easy
#' graphing function that makes best use of the data,
#' whatever its composition. Users can also tailor the plot according to their 
#' preferences regarding node size, colour, and shape. The function also supports
#' visualisation of network measures such as centrality.
#' @param object A migraph-consistent object.
#' @param layout An igraph layout algorithm,
#'   currently defaults to 'stress'.
#' @param labels Logical, whether to print node names
#'   as labels if present.
#' @param node_shape Character string in quotation marks referring to the name 
#'   of a node attribute already present in the graph to be used for the shapes 
#'   of the nodes. 
#'   Shapes follow the ordering "circle", "square", "triangle",
#'   so this aesthetic should be used for a variable with only a few categories.
#' @param node_size Node variable in quotation marks to be used for 
#'   the size of the nodes.
#'   This can be any continuous variable on the nodes of the network.
#'   Since this function expects this to be an existing variable,
#'   it is recommended to calculate all node-related statistics prior
#'   to using this function.
#' @param node_color Node variable in quotation marks to be used for 
#'   coloring the nodes. It is easiest if this is added as a node attribute to
#'   the graph before plotting.
#' @param node_group Node variable in quotation marks to be used for
#'   drawing convex but also concave hulls around clusters of nodes.
#'   These groupings will be labelled with the categories of the variable passed.
#' @param highlight_measure String vector of name of the node and/or edge
#' level measure function e.g. `node_degree`,  `edge_betweenness`,
#' or `c("node_betweenness", "edge_betweenness")`.
#' `NULL` by default.
#' @param identify_function Name of the function used to determine the
#' highlighted node e.g. `"max"`, `"min"`, or `c("max", "min")` if
#' both edge and node highlighting is performed. `NULL` by default.
#' @param ... Extra arguments.
#' @importFrom ggraph create_layout ggraph geom_edge_link geom_node_text
#' @importFrom ggraph geom_conn_bundle get_con geom_node_point
#' @importFrom ggraph scale_edge_width_continuous geom_node_label
#' @importFrom igraph get.vertex.attribute
#' @importFrom ggplot2 aes arrow unit scale_color_brewer scale_fill_brewer
#' @importFrom ggforce geom_mark_hull
#' @import concaveman
#' @examples
#' ison_adolescents <- ison_adolescents %>% 
#'  dplyr::mutate(shape = rep(c("circle", "square"), times = 4)) %>%
#'  dplyr::mutate(color = rep(c("blue", "red"), times = 4))
#' autographr(ison_adolescents, node_shape = "shape", node_color = "color")
#' autographr(ison_karateka, node_size = rep(c(0.8), times = 34))
#' # Node highlighting:
#' autographr(ison_adolescents, highlight_measure = "node_betweenness", identify_function = "max")
#' # Edge highlighting:
#' autographr(ison_adolescents, highlight_measure = "edge_betweenness", identify_function = "max")
#' # Both node and edge highlighting:
#' autographr(ison_adolescents, highlight_measure = c("node_betweenness", 
#' "edge_betweenness"), identify_function = c("max", "max"))
#' @export
autographr <- auto_graph <- function(object,
                                     layout = "stress",
                                     labels = TRUE,
                                     node_color = NULL,
                                     node_group = NULL,
                                     node_shape = NULL,
                                     node_size = NULL,
                                     highlight_measure = NULL,
                                     identify_function = NULL,
                                     ...) {
  
  name <- weight <- NULL # initialize variables to avoid CMD check notes
  g <- as_tidygraph(object)
  # Get the highlight measure ----
  if (!is.null(highlight_measure) & !is.null(identify_function)) {
    # There is something to be highlighted
    if (length(identify_function) == length(highlight_measure)) {
      if (any(grepl("node", highlight_measure))) {
        # List of node measure functions
        node_measure_name <- highlight_measure[grepl("node", highlight_measure)]
        node_measure <- lapply(node_measure_name, get)
        node_identify_name <- identify_function[grepl("node", highlight_measure)]
        node_identify_measure <- lapply(node_identify_name, get)
      }
      if (any(grepl("edge", highlight_measure))) {
        # List of edge measure functions
        edge_measure_name <- highlight_measure[grepl("edge", highlight_measure)]
        edge_measure <- lapply(edge_measure_name, get)
        edge_identify_name <- identify_function[grepl("edge", highlight_measure)]
        edge_identify_measure <- lapply(edge_identify_name, get)
      }
    } else {
      stop("If a highlighting measure is specified, an identification function
           must be specified as well with the same number of elments.")
    }
  } else {
    # Nothing gets highlighted
    highlight_measure <- "NA"
  }
  # Add layout ----
  lo <- ggraph::create_layout(g, layout)
  if ("graph" %in% names(attributes(lo))) {
    if (!setequal(names(as.data.frame(attr(lo, "graph"))), names(lo))) {
      for (n in setdiff(names(as.data.frame(attr(lo, "graph"))), names(lo))) {
        lo[n] <- igraph::get.vertex.attribute(g, n)
      }
    } 
  }
  p <- ggraph::ggraph(lo) + ggplot2::theme_void()
  # Add Edge Highlight Attribute if relevant ----
  if (any(grepl("edge", highlight_measure))) {
    # Measure; needs to be an edge level measure
    em <- edge_measure[[1]]
    em_if <- edge_identify_measure[[1]]
    measure <- em(g)
    # Add as attribute
    g <- add_edge_attributes(g, "highlight_measure_edge",
                             ifelse(measure == em_if(measure),
                                    gsub(pattern = '.*["]([^.]+)["].*', "\\1",
                                         deparse(identify_function)), "other"))
    # Let the rest of the function know that it needs to color things according
    # to the highlight_measure attribute.
    edge_color <- "highlight_measure_edge"
  }  
  # Add edges ----
  if (is_signed(g)) {
    edge_linetype <- ifelse(igraph::E(g)$sign >= 0, "solid", "dashed")
    edge_colour <- ifelse(igraph::E(g)$sign >= 0, "#0072B2", "#E20020")
    } else if (any(grepl("edge", highlight_measure))) {
      edge_colour <- ifelse(igraph::E(g)$highlight_measure_edge != "other",
                            "#E20020", "#0072B2")
      edge_linetype <- "solid"
    } else if (igraph::gsize(g) == 0) {
      # Edge case where there are no edges
      edge_linetype <- NULL
      edge_colour <- NULL
    } else {
      edge_linetype <- "solid"
      edge_colour <- "black"
  }
  # Begin plotting edges in various cases ----
  if (is_directed(g)) {
    if (is_weighted(g)) {
      p <- p + ggraph::geom_edge_link(ggplot2::aes(width = weight,
                                                   colour = edge_colour),
                                      edge_alpha = 0.4,
                                      edge_linetype = edge_linetype,
                                      arrow = ggplot2::arrow(angle = 15,
                                                             length = ggplot2::unit(2, 'mm'),
                                                             type = "closed"), 
                                      end_cap = ggraph::circle(1.5, 'mm')) +
        ggraph::scale_edge_width_continuous(range = c(0.2, 2.5), 
                                            guide = "none")
    } else {
      p <- p + ggraph::geom_edge_link(ggplot2::aes(colour = edge_colour),
                                      edge_alpha = 0.4,
                                      edge_linetype = edge_linetype,
                                      arrow = ggplot2::arrow(angle = 15,
                                                    length = ggplot2::unit(3, "mm"),
                                                    type = "closed"),
                                      end_cap = ggraph::circle(3, "mm"))
    }
  } else {
    if (is_weighted(g)) {
      p <- p + ggraph::geom_edge_link0(ggplot2::aes(width = weight,
                                                    colour = edge_colour),
                                       edge_linetype = edge_linetype,
                                       edge_alpha = 0.4) + 
        ggraph::scale_edge_width_continuous(range = c(.2, 1),
                                    guide = "none")
    } else {
      p <- p + ggraph::geom_edge_link0(ggplot2::aes(colour = edge_colour),
                                       edge_linetype = edge_linetype,
                                       edge_alpha = 0.4)
    }
  }
  
  # Node size
  if (!is.null(node_size)) {
    if (is.character(node_size)) {
      nsize <- node_attribute(g, node_size)
    } else if (is.numeric(node_size)) {
      nsize <- node_size
    } else {
      nsize <- node_size(g)
    }
  } else {
    nsize <- ifelse(graph_nodes(g) <= 10, 5, (100 / graph_nodes(g)) / 2)
  }
  # Add node highlighting if relevant ----
  if (any(grepl("node", highlight_measure))) {
    # Measure; needs to be a node level measure
    nm <- node_measure[[1]]
    nm_if <- node_identify_measure[[1]]
    measure <- nm(g)
    # Add as attribute
    g <- add_node_attributes(g, "highlight_measure_node",
                             ifelse(measure == nm_if(measure),
                                    gsub(pattern = '.*["]([^.]+)["].*', "\\1",
                                         deparse(identify_function)), "other"))
    # Let the rest of the function know that it needs to color things according
    # to the highlight_measure attribute.
    node_color <- "highlight_measure_node"
  }
  # Add nodes ----
  if (!is.null(node_shape)) {
    node_shape <- as.factor(igraph::get.vertex.attribute(g, node_shape))
    node_shape <- c("circle","square","triangle")[node_shape]
  } else if (is_twomode(g)) {
    node_shape <- ifelse(igraph::V(g)$type,
                         "square",
                         "circle")
  } else {
    node_shape <- "circle"
  }
  if (is_twomode(g)) {
    if (!is.null(node_color)) {
      color_factor_node <- as.factor(igraph::get.vertex.attribute(g, node_color))
      p <- p + ggraph::geom_node_point(ggplot2::aes(color = color_factor_node),
                               size = nsize,
                               shape = node_shape) +
        ggplot2::scale_colour_brewer(palette = "Set1",
                            guide = "none")
    } else {
      p <- p + ggraph::geom_node_point(size = nsize,
                               shape = node_shape)
    }
  } else {
    if (!is.null(node_color)) {
      color_factor_node <- as.factor(igraph::get.vertex.attribute(g, node_color))
      p <- p + ggraph::geom_node_point(aes(color = color_factor_node),
                               size = nsize,
                               shape = node_shape) +
        ggplot2::scale_colour_brewer(palette = "Set1", guide = "none")
    } else {
      p <- p + ggraph::geom_node_point(size = nsize,
                                       shape = node_shape)
    }
  }
  # Plot one mode
  if (labels & is_labelled(g) & !is_twomode(g)) {
    p <- p + ggraph::geom_node_label(ggplot2::aes(label = name),
                                     label.padding = 0.15,
                                     label.size = 0,
                                     repel = TRUE)
  p
  }
  # Plot two modes
  if (labels & is_labelled(g) & is_twomode(g)) {
    p <- p + ggraph::geom_node_label(ggplot2::aes(label = name),
                                     label.padding = 0.15,
                                     label.size = 0,
                                     # fontface = ifelse(igraph::V(g)$type,
                                     #                   "bold",
                                     #                   "plain"),
                                     # size = ifelse(igraph::V(g)$type,
                                     #               4,
                                     #               3),
                                     hjust = "inward",
                                     repel = TRUE)
  p
  }
  if (!is.null(node_group)) {
    p <- p + ggforce::geom_mark_hull(ggplot2::aes(x = lo$x, y = lo$y,
                                         fill = as.factor(igraph::get.vertex.attribute(g, node_group)),
                                         label = as.factor(igraph::get.vertex.attribute(g, node_group))),
                                     concavity = 2) +
      ggplot2::scale_fill_brewer(palette = "Set1", guide = "none")
  }
  p
}

# autographr <- function()
# {
#   .Deprecated("auto_graph")
# }

