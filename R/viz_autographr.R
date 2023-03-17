#' Quickly graph networks with sensible defaults
#' 
#' @description 
#' The aim of this function is to provide users with a quick and easy
#' graphing function that makes best use of the data,
#' whatever its composition. Users can also tailor the plot according to their 
#' preferences regarding node size, colour, and shape. The function also supports
#' visualisation of network measures such as centrality.
#' @family mapping
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
#' @param edge_color Tie variable in quotation marks to be used for 
#'   coloring the nodes. It is easiest if this is added as an edge or tie attribute 
#'   to the graph before plotting.
#' @param node_group Node variable in quotation marks to be used for
#'   drawing convex but also concave hulls around clusters of nodes.
#'   These groupings will be labelled with the categories of the variable passed.
#' @param ... Extra arguments to pass on to `autographr()`/`ggraph()`/`ggplot()`.
#' @importFrom ggraph geom_edge_link geom_node_text geom_conn_bundle
#' get_con geom_node_point scale_edge_width_continuous geom_node_label
#' @importFrom ggplot2 aes arrow unit scale_color_brewer scale_fill_brewer
#' @importFrom ggforce geom_mark_hull
#' @name auto_graph
NULL

#' @describeIn auto_graph Graphs a network with sensible defaults
#' @examples
#' ison_adolescents %>% 
#'   mutate(shape = rep(c("circle", "square"), times = 4)) %>%
#'   mutate(color = rep(c("blue", "red"), times = 4)) %>%
#'   autographr(node_shape = "shape", node_color = "color")
#' autographr(ison_karateka, node_size = 8)
#' ison_adolescents %>% 
#'   mutate(high_degree = node_is_max(node_degree())) %>% 
#'   activate(edges) %>% 
#'   mutate(high_betweenness = tie_is_max(tie_betweenness(ison_adolescents))) %>% 
#'   autographr(node_color = "high_degree", edge_color = "high_betweenness")
#' autographr(mpn_elite_usa_advice, "concentric")
#' @export
autographr <- function(object,
                       layout = "stress",
                       labels = TRUE,
                       node_color = NULL,
                       node_group = NULL,
                       node_shape = NULL,
                       node_size = NULL,
                       edge_color = NULL,
                       ...) {
  
  name <- weight <- nodes <- NULL #initialize variables to avoid CMD check notes
  g <- as_tidygraph(object)
  if(!is.null(node_group)) {
    node_group <- as.factor(node_attribute(g, node_group))
    g <- as_tidygraph(g) %>% 
      tidygraph::activate(nodes) %>%
      dplyr::mutate(node_group = node_group)
  }
  
  # Add layout ----
  p <- .graph_layout(g, layout, labels, node_group)
  
  # Add edges ----
  p <- .graph_edges(p, g, edge_color)
  
  # Add nodes ----
  p <- .graph_nodes(p, g, node_color, node_shape, node_size)
  
  p
}

#' @describeIn auto_graph Graphs a list of networks 
#'   with sensible defaults
#' @param netlist A list of migraph-compatible networks.
#' @importFrom patchwork wrap_plots
#' @source http://blog.schochastics.net/post/animating-network-evolutions-with-gganimate/
#' @examples
#'   autographs(to_egos(ison_adolescents))
#' @export
autographs <- function(netlist, ...) {
  if(!is.null(names(netlist))){
    gs <- lapply(1:length(netlist), function(x)
      autographr(netlist[[x]], ...) +
        ggtitle(names(netlist)[x]))
  } else {
    gs <- lapply(netlist, function(x)
      autographr(x, ...))
  }
  do.call(patchwork::wrap_plots, gs)
}

#' @describeIn auto_graph Graphs an dynamic (animated) network
#'   with sensible defaults
#' @param tlist The same migraph-compatible network listed according to
#'   a time attribute, waves, or slices.
#' @param keep_isolates Would you like to remove vertices that do not have
#'   any adjacent edges in each frame?
#'   TRUE by default.
#'   If FALSE, deletes isolated vertices in each frame.
#' @details If layout is "dynamic", the function reates a dynamic layout for a
#' network in time with ´{graphlayouts}´. Not all `{ggraph}` layouts work,
#' some options here are "stress", "circle", "kk", and "drl".
#' Plots are animated with the help of ´{gganimate}´.
#' @importFrom igraph gsize as_data_frame get.edgelist
#' @importFrom ggplot2 ggplot geom_segment geom_point geom_text
#' scale_alpha_manual theme_void
#' @importFrom gganimate transition_states ease_aes
#' @importFrom graphlayouts layout_as_dynamic
#' @importFrom ggraph create_layout
#' @importFrom dplyr mutate select distinct left_join
#' @source http://blog.schochastics.net/post/animating-network-evolutions-with-gganimate/
#' @examples
#' ison_adolescents %>%
#'   activate(edges) %>%
#'   mutate(year = sample(1995:1998, 10, replace = TRUE)) %>%
#'   to_waves(attribute = "year") %>%
#'   autographd()
#' ison_adolescents %>%
#'   mutate(shape = rep(c("circle", "square"), times = 4),
#'          color = rep(c("blue", "red"), times = 4),
#'          size = sample(4:8, 8, replace = TRUE)) %>%
#'   activate(edges) %>%
#'   mutate(year = sample(1995:1998, 10, replace = TRUE),
#'          e_color = sample(c("orange", "green"), 10, replace = TRUE)) %>%
#'   to_waves(attribute = "year") %>%
#'   autographd(keep_isolates = FALSE, layout = "kk", node_shape = "shape",
#'              node_color = "color", node_size =  "size", edge_color = "e_color")
#' @export
autographd <- function(tlist, keep_isolates = TRUE, layout = "dynamic",
                       label = TRUE, node_color = NULL, node_shape = NULL,
                       node_size = NULL, edge_color = NULL, ...) {

  # Todo: make code more concise and setup helper functions
  # Todo: add extra (...) arguments passed on to `ggraph()`/`ggplot()`/`gganimate()`

  # Check if object is a list of lists
  if (!is.list(tlist[[1]])) {
    stop("Please declare a migraph-compatible network listed according
         to a time attribute, waves, or slices.")
  }
  # Remove lists without edges
  tlist <- Filter(function(x) igraph::gsize(x) > 0, tlist)
  # Create an edge list
  edges_lst <- lapply(1:length(tlist), function(i)
    cbind(igraph::as_data_frame(tlist[[i]], "edges"), frame = i))
  # Check if all names are present in all lists
  if (length(unique(unlist(unname(lapply(tlist, length))))) != 1) {
    tlist <- to_waves(do.call("rbind", edges_lst), attribute = "frame")
  }
  # Add separate layouts for each time point
  if (layout == "dynamic") {
    require(igraph, quietly = TRUE)
    lay <- graphlayouts::layout_as_dynamic(tlist, alpha = 0.2)
  } else {
    lay <- lapply(1:length(tlist), function(i)
      ggraph::create_layout(tlist[[i]], layout))
  }
  # Create a node list for each time point
  nodes_lst <- lapply(1:length(tlist), function(i) {
    cbind(igraph::as_data_frame(tlist[[i]], "vertices"),
          x = lay[[i]][, 1], y = lay[[i]][, 2], frame = i)
  })
  # Create an edge list for each time point
  edges_lst <- lapply(1:length(tlist), function(i) {
    edges_lst[[i]]$x <- nodes_lst[[i]]$x[match(edges_lst[[i]]$from,
                                               nodes_lst[[i]]$name)]
    edges_lst[[i]]$y <- nodes_lst[[i]]$y[match(edges_lst[[i]]$from,
                                               nodes_lst[[i]]$name)]
    edges_lst[[i]]$xend <- nodes_lst[[i]]$x[match(edges_lst[[i]]$to,
                                                  nodes_lst[[i]]$name)]
    edges_lst[[i]]$yend <- nodes_lst[[i]]$y[match(edges_lst[[i]]$to,
                                                  nodes_lst[[i]]$name)]
    edges_lst[[i]]$id <- paste0(edges_lst[[i]]$from, "-", edges_lst[[i]]$to)
    edges_lst[[i]]$status <- TRUE
    edges_lst[[i]]
  })
  # Get edge IDs for all edges
  all_edges <- do.call("rbind", lapply(tlist, igraph::get.edgelist))
  all_edges <- all_edges[!duplicated(all_edges), ]
  all_edges <- cbind(all_edges, paste0(all_edges[, 1], "-", all_edges[, 2]))
  # Keep only necessary columns
  edges_lst <- lapply(edges_lst, function (x) x[,c("from", "to", "frame", "x",
                                                   "y", "xend", "yend", "id",
                                                   "status", edge_color)])
  # Add edges level information for edge transitions
  edges_lst <- lapply(1:length(tlist), function(i) {
    idx <- which(!all_edges[, 3] %in% edges_lst[[i]]$id)
    if (length(idx != 0)) {
      tmp <- data.frame(from = all_edges[idx, 1], to = all_edges[idx, 2],
                        id = all_edges[idx, 3])
      tmp$x <- nodes_lst[[i]]$x[match(tmp$from, nodes_lst[[i]]$name)]
      tmp$y <- nodes_lst[[i]]$y[match(tmp$from, nodes_lst[[i]]$name)]
      tmp$xend <- nodes_lst[[i]]$x[match(tmp$to, nodes_lst[[i]]$name)]
      tmp$yend <- nodes_lst[[i]]$y[match(tmp$to, nodes_lst[[i]]$name)]
      tmp$frame <- i
      tmp$status <- FALSE
      edges_lst[[i]] <- dplyr::bind_rows(edges_lst[[i]], tmp)
    }
    edges_lst[[i]]
  })
  # Bind edges list
  edges_out <- do.call("rbind", edges_lst)
  # Remove NAs in edge color column, if declared
  if (!is.null(edge_color)) {
    which(colnames(df)==edge_color)
    edges_out[edge_color] <- ifelse(is.na(edges_out[[edge_color]]), "black",
                                    edges_out[[edge_color]])
  }
  # Bind nodes list
  nodes_out <- do.call("rbind", nodes_lst)
  # Delete nodes for each frame if isolate
  if (isFALSE(keep_isolates)) {
    # Create node metadata for node presence in certain frame
    meta <- edges_out %>%
      dplyr::filter(status == TRUE) %>%
      dplyr::mutate(meta = ifelse(frame > 1, paste0(from, (frame - 1)), from)) %>%
      dplyr::select(meta, status) %>%
      dplyr::distinct()
    metab <- edges_out %>%
      dplyr::filter(status == TRUE) %>%
      dplyr::mutate(meta = ifelse(frame > 1, paste0(to, (frame - 1)), to)) %>%
      dplyr::select(meta, status) %>%
      rbind(meta) %>%
      dplyr::distinct()
    # Mark nodes that are isolates
    nodes_out$meta <- rownames(nodes_out)
    # Join data
    nodes_out <- dplyr::left_join(nodes_out, metab, by = "meta") %>%
      dplyr::mutate(status = ifelse(is.na(status), FALSE, TRUE)) %>%
      dplyr::distinct()
  } else {
    if(nrow(nodes_out)/length(unique(nodes_out$frame)) > 20) {
      message("Please considering deleting isolates to improve visualisation.")
    } 
    nodes_out$status <- TRUE
  }
  # Plot with ggplot2 and gganimate
  p <- ggplot2::ggplot()
  # Plot edges
  if (!is.null(edge_color)) {
    edge_color <- as.factor(edges_out[[edge_color]])
    color <- colors()
    if(!any(grepl(paste(color, collapse = "|"), edge_color)) |
            any(grepl("#", edge_color))) {
      for(i in unique(edge_color)) {
        edge_color[edge_color == i] <- sample(color, 1)
      }
    }
  } else {
    edge_color <- rep("black", nrow(edges_out))
  }
  p <- p + ggplot2::geom_segment(data = edges_out,
                                 aes(x = x, xend = xend, y = y, yend = yend,
                                     group = id, alpha = status),
                                 color = edge_color,
                                 show.legend = FALSE)
  # Set node shape, color, and size
  if (!is.null(node_shape)) {
    node_shape <- as.factor(nodes_out[[node_shape]])
    node_shape <- c("circle","square","triangle")[node_shape]
  } else {
    node_shape <- rep("circle", nrow(nodes_out))
  }
  if (!is.null(node_color)) {
    node_color <- as.factor(nodes_out[[node_color]])
    color <- colors()
    if(!any(grepl(paste(color, collapse = "|"), node_color)) |
       any(grepl("#", node_color))) {
      for(i in unique(node_color)) {
        node_color[node_color == i] <- sample(color, 1)
      }
    }
  } else {
    node_color <- rep("darkgray", nrow(nodes_out))
  }
  if (!is.null(node_size)) {
    node_size <- as.factor(as.numeric(nodes_out[[node_size]]))
  } else {
    node_size <- rep(nrow(nodes_out)/length(unique(nodes_out$frame)),
                     nrow(nodes_out))
  }
  # Plot nodes
  p <- p + ggplot2::geom_point(data = nodes_out,
                               aes(x, y, group = name, alpha = status),
                               size = node_size, color = node_color,
                               shape = node_shape, show.legend = FALSE)
  if (isTRUE(label)) {
    p <- p +  ggplot2::geom_text(data = nodes_out,
                                 aes(x, y, label =  name, alpha = status),
                                 hjust = -0.2, vjust = -0.2, show.legend = FALSE)
  }
  p + ggplot2::scale_alpha_manual(values = c(0, 1)) +
    gganimate::transition_states(frame, state_length = 1) +
    ggplot2::labs(title = "{closest_state}") +
    ggplot2::theme_void()
}

#' @importFrom ggraph create_layout ggraph
#' @importFrom igraph get.vertex.attribute
#' @importFrom ggplot2 theme_void
.graph_layout <- function(g, layout, labels, node_group){
  name <- NULL
  lo <- ggraph::create_layout(g, layout)
  if ("graph" %in% names(attributes(lo))) {
    if (!setequal(names(as.data.frame(attr(lo, "graph"))), names(lo))) {
      for (n in setdiff(names(as.data.frame(attr(lo, "graph"))), names(lo))) {
        lo[n] <- igraph::get.vertex.attribute(g, n)
      }
    } 
  }
  p <- ggraph::ggraph(lo) + ggplot2::theme_void()
  if (labels & is_labelled(g)){
    if(layout %in% c("concentric", "circle")){
      # https://stackoverflow.com/questions/57000414/ggraph-node-labels-truncated?rq=1
      angles <- as.data.frame(cart2pol(as.matrix(lo[,1:2])))
      angles$degree <- angles$phi * 180/pi
      angles <- dplyr::case_when(lo[,2] >= 0 & lo[,1] > 0 ~ angles$degree, 
                                 lo[,2] < 0 & lo[,1] > 0 ~ angles$degree,
                                 lo[,1] == 1 ~angles$degree,
                                 TRUE ~ angles$degree - 180)
      hj <- ifelse(lo[,1] > 0, -0.2, 1.2)
      p <- p + ggraph::geom_node_text(ggplot2::aes(label = name),
                                      size = 2,
                                      hjust = hj,
                                      angle = angles) +
        ggplot2::coord_cartesian(xlim=c(-1.2,1.2), ylim=c(-1.2,1.2))
    } else if(layout %in% c("bipartite", "railway") | 
              (layout == "hierarchy" & length(unique(lo[,2])) <= 2)){
      p <- p + ggraph::geom_node_text(ggplot2::aes(label = name),
                                      size = 2,
                                      hjust = "outward",
                                      nudge_y = ifelse(lo[,2] == 1, 0.05, -0.05),
                                      # vjust = ifelse(node_mode(object), -1, 1),
                                      angle = 90) +
        ggplot2::coord_cartesian(ylim=c(-0.2,1.2))
    } else if(!is_twomode(g)) { # Plot one mode
      p <- p + ggraph::geom_node_label(ggplot2::aes(label = name),
                                       label.padding = 0.15,
                                       label.size = 0,
                                       repel = TRUE,
                                       seed = 1234)
    } else { # Plot two modes
      p <- p + ggraph::geom_node_text(ggplot2::aes(label = name),
                                      repel = TRUE,
                                      size = 2,
                                      hjust = "outward",
                                      nudge_x = ifelse(lo[,1] == 1, 0.05, -0.05),
                                      seed = 1234)
    }
  }
  
  if (!is.null(node_group)) p <- .graph_groups(p, g, node_group, lo)
  
  p
}

.graph_edges <- function(p, g, edge_color){
  # if (is_signed(g)) {
  #   edge_linetype <- ifelse(igraph::E(g)$sign >= 0, "solid", "dashed")
  #   edge_color <- ifelse(igraph::E(g)$sign >= 0, "#0072B2", "#E20020")
  #   } else if (!is.null(edge_color)) {
  #     edge_color <- as.factor(igraph::get.edge.attribute(g, edge_color))
  #     edge_color <- ifelse(edge_color != levels(edge_color)[1], "#e41a1c", "#377eb8")
  #     edge_linetype <- "solid"
  #   } else if (igraph::gsize(g) == 0) {
  #     # Edge case where there are no edges
  #     edge_linetype <- NULL
  #     edge_color <- NULL
  #   } else {
  #     edge_linetype <- "solid"
  #     edge_color <- "black"
  # }
  weight <- NULL
  # Begin plotting edges in various cases
  if (is_directed(g)) {
    if (is_weighted(g)) {
      if (!is.null(edge_color)) {
        edge_color <- as.factor(tie_attribute(g, edge_color))
        p <- p + ggraph::geom_edge_link(ggplot2::aes(width = weight,
                                                     colour = edge_color),
                                        edge_alpha = 0.4,
                                        edge_linetype = "solid",
                                        arrow = ggplot2::arrow(angle = 15,
                                                               length = ggplot2::unit(2, 'mm'),
                                                               type = "closed"), 
                                        end_cap = ggraph::circle(1.5, 'mm')) +
          ggraph::scale_edge_width_continuous(range = c(0.2, 2.5), 
                                              guide = "none") +
          ggraph::scale_edge_colour_brewer(palette = "Set1",
                                           direction = -1,
                                           guide = "none")
      } else if (is_signed(g)) {
        edge_color <- ifelse(igraph::E(g)$sign >= 0, "#0072B2", "#E20020")
        edge_linetype <- ifelse(igraph::E(g)$sign >= 0, "solid", "dashed")
        p <- p + ggraph::geom_edge_link(ggplot2::aes(width = weight,
                                                     colour = edge_color,
                                                     linetype = edge_linetype),
                                        edge_alpha = 0.4,
                                        arrow = ggplot2::arrow(angle = 15,
                                                               length = ggplot2::unit(2, 'mm'),
                                                               type = "closed"), 
                                        end_cap = ggraph::circle(1.5, 'mm')) +
          ggraph::scale_edge_width_continuous(range = c(0.2, 2.5), 
                                              guide = "none")
      } else {
        p <- p + ggraph::geom_edge_link(ggplot2::aes(width = weight),
                                        edge_colour = "black",
                                        edge_alpha = 0.4,
                                        edge_linetype = "solid",
                                        arrow = ggplot2::arrow(angle = 15,
                                                               length = ggplot2::unit(2, 'mm'),
                                                               type = "closed"), 
                                        end_cap = ggraph::circle(1.5, 'mm')) +
          ggraph::scale_edge_width_continuous(range = c(0.2, 2.5), 
                                              guide = "none")
      }
    } else {
      if (!is.null(edge_color)) {
        edge_color <- as.factor(tie_attribute(g, edge_color))
        p <- p + ggraph::geom_edge_link(ggplot2::aes(colour = edge_color),
                                        edge_alpha = 0.4,
                                        edge_linetype = "solid",
                                        arrow = ggplot2::arrow(angle = 15,
                                                               length = ggplot2::unit(3, "mm"),
                                                               type = "closed"),
                                        end_cap = ggraph::circle(3, "mm")) +
          ggraph::scale_edge_colour_brewer(palette = "Set1",
                                           direction = -1,
                                           guide = "none")
      } else if (is_signed(g)) {
        edge_color <- ifelse(igraph::E(g)$sign >= 0, "#0072B2", "#E20020")
        edge_linetype <- ifelse(igraph::E(g)$sign >= 0, "solid", "dashed")
        p <- p + ggraph::geom_edge_link(ggplot2::aes(colour = edge_color,
                                                     linetype = edge_linetype),
                                        edge_alpha = 0.4,
                                        arrow = ggplot2::arrow(angle = 15,
                                                               length = ggplot2::unit(3, "mm"),
                                                               type = "closed"),
                                        end_cap = ggraph::circle(3, "mm"))
      } else {
        p <- p + ggraph::geom_edge_link(edge_colour = "black",
                                        edge_alpha = 0.4,
                                        edge_linetype = "solid",
                                        arrow = ggplot2::arrow(angle = 15,
                                                               length = ggplot2::unit(3, "mm"),
                                                               type = "closed"),
                                        end_cap = ggraph::circle(3, "mm"))
      }
    }
  } else {
    if (is_weighted(g)) { # weighted and undirected
      if (!is.null(edge_color)) {
        edge_color <- as.factor(tie_attribute(g, edge_color))
        p <- p + ggraph::geom_edge_link(ggplot2::aes(width = weight,
                                                     colour = edge_color),
                                        edge_alpha = 0.4,
                                        edge_linetype = "solid") +
          ggraph::scale_edge_width_continuous(range = c(0.2, 1), 
                                              guide = "none") +
          ggraph::scale_edge_colour_brewer(palette = "Set1",
                                           direction = -1,
                                           guide = "none")
      } else if (is_signed(g)) {
        edge_color <- ifelse(igraph::E(g)$sign >= 0, "#0072B2", "#E20020")
        edge_linetype <- ifelse(igraph::E(g)$sign >= 0, "solid", "dashed")
        p <- p + ggraph::geom_edge_link(ggplot2::aes(width = weight,
                                                     colour = edge_color,
                                                     linetype = edge_linetype),
                                        edge_alpha = 0.4) +
          ggraph::scale_edge_width_continuous(range = c(0.2, 1), 
                                              guide = "none")
      } else {
        p <- p + ggraph::geom_edge_link0(ggplot2::aes(width = weight),
                                         edge_colour = "black",
                                         edge_linetype = "solid",
                                         edge_alpha = 0.4) + 
          ggraph::scale_edge_width_continuous(range = c(.2, 1),
                                              guide = "none")
      }
    } else { # unweighted and undirected
      if (!is.null(edge_color)) {
        edge_color <- as.factor(tie_attribute(g, edge_color))
        p <- p + ggraph::geom_edge_link0(ggplot2::aes(colour = edge_color),
                                         edge_linetype = "solid",
                                         edge_alpha = 0.4) +
          ggraph::scale_edge_colour_brewer(palette = "Set1",
                                           direction = -1,
                                           guide = "none")
      } else if (is_signed(g)) {
        edge_color <- ifelse(igraph::E(g)$sign >= 0, "#0072B2", "#E20020")
        edge_linetype <- ifelse(igraph::E(g)$sign >= 0, "solid", "dashed")
        p <- p + ggraph::geom_edge_link(ggplot2::aes(colour = edge_color,
                                                     linetype = edge_linetype),
                                        edge_alpha = 0.4)
      } else {
        p <- p + ggraph::geom_edge_link0(ggplot2::aes(colour = edge_color),
                                         edge_linetype = "solid",
                                         edge_alpha = 0.4) 
      }
    }
  }
}

.graph_nodes <- function(p, g, node_color, node_shape, node_size){
  if (!is.null(node_size)) {
    if (is.character(node_size)) {
      nsize <- node_attribute(g, node_size)
    } else if (is.numeric(node_size)) {
      nsize <- node_size
    } else {
      nsize <- node_size(g)
    }
  } else {
    nsize <- ifelse(network_nodes(g) <= 10, 5, (100 / network_nodes(g)) / 2)
  }
  
  if (!is.null(node_shape)) {
    node_shape <- as.factor(node_attribute(g, node_shape))
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
      color_factor_node <- as.factor(node_attribute(g, node_color))
      p <- p + ggraph::geom_node_point(ggplot2::aes(color = color_factor_node),
                                       size = nsize,
                                       shape = node_shape) +
        ggplot2::scale_colour_brewer(palette = "Set1",
                                     direction = ifelse(length(unique(color_factor_node))==2,
                                                        -1,1),
                                     guide = "none")
    } else {
      p <- p + ggraph::geom_node_point(size = nsize,
                                       shape = node_shape)
    }
  } else {
    if (!is.null(node_color)) {
      color_factor_node <- as.factor(node_attribute(g, node_color))
      p <- p + ggraph::geom_node_point(aes(color = color_factor_node),
                                       size = nsize,
                                       shape = node_shape) +
        ggplot2::scale_colour_brewer(palette = "Set1",
                                     direction = ifelse(length(unique(color_factor_node))==2,
                                                        -1,1),
                                     guide = "none")
    } else {
      p <- p + ggraph::geom_node_point(size = nsize,
                                       shape = node_shape)
    }
  }
  p
}

.graph_groups <- function(p, g, node_group, lo){
  if (!("concaveman" %in% rownames(utils::installed.packages()))) {
    message("Please install package `{concaveman}`.")
  } else {
    p <- p + ggforce::geom_mark_hull(ggplot2::aes(x = lo$x, y = lo$y,
                                                  fill = node_group,
                                                  label = node_group),
                                     concavity = 2) +
      ggplot2::scale_fill_brewer(palette = "Set1", guide = "none")
  }
}

cart2pol <- function(xyz){
  stopifnot(is.numeric(xyz))
  if (is.vector(xyz) && (length(xyz) == 2 || length(xyz) == 
                         3)) {
    x <- xyz[1]
    y <- xyz[2]
    m <- 1
    n <- length(xyz)
  }
  else if (is.matrix(xyz) && (ncol(xyz) == 2 || ncol(xyz) == 
                              3)) {
    x <- xyz[, 1]
    y <- xyz[, 2]
    m <- nrow(xyz)
    n <- ncol(xyz)
  }
  else stop("Input must be a vector of length 3 or a matrix with 3 columns.")
  phi <- atan2(y, x)
  r <- hypot(x, y)
  if (n == 2) {
    if (m == 1) 
      prz <- c(phi, r)
    else prz <- cbind(phi, r)
  }
  else {
    if (m == 1) {
      z <- xyz[3]
      prz <- c(phi, r, z)
    }
    else {
      z <- xyz[, 3]
      prz <- cbind(phi, r, z)
    }
  }
  return(prz)
}

hypot <- function (x, y) {
  if ((length(x) == 0 && is.numeric(y) && length(y) <= 1) || 
      (length(y) == 0 && is.numeric(x) && length(x) <= 1)) 
    return(vector())
  if (!is.numeric(x) && !is.complex(x) || !is.numeric(y) && 
      !is.complex(y)) 
    stop("Arguments 'x' and 'y' must be numeric or complex.")
  if (length(x) == 1 && length(y) > 1) {
    x <- rep(x, length(y))
    dim(x) <- dim(y)
  }
  else if (length(x) > 1 && length(y) == 1) {
    y <- rep(y, length(x))
    dim(y) <- dim(x)
  }
  if ((is.vector(x) && is.vector(y) && length(x) != length(y)) || 
      (is.matrix(x) && is.matrix(y) && dim(x) != dim(y)) || 
      (is.vector(x) && is.matrix(y)) || is.matrix(x) && is.vector(y)) 
    stop("Arguments 'x' and 'y' must be of the same size.")
  x <- abs(x)
  y <- abs(y)
  m <- pmin(x, y)
  M <- pmax(x, y)
  ifelse(M == 0, 0, M * sqrt(1 + (m/M)^2))
}
