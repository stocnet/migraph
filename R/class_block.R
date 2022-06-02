#' Blockmodelling
#' Passing the function a list apply the algorithm to the joined matrices.
#' @inheritParams is
#' @param block.content A string indicating which method to use for
#' calculating block content.
#' Options are: "density", "sum", "meanrowsum", "meancolsum",
#' "median", "min", "max".
#' @param block_labels A character vector manually providing labels
#' for the blocks in the blockmodel
#' @param clusters the vector of cluster membership for the blockmodel
#' @param blockmodel a blockmodel object
#' @name blockmodel
NULL

#' @rdname blockmodel
#' @importFrom sna blockmodel
#' @examples 
#' blockmodel(ison_southern_women, membership)
#' @export
blockmodel <- function(object, clusters){
  # if(is_twomode(object)) object <- to_onemode(object)
  mat <- as_matrix(to_onemode(object))
  out <- sna::blockmodel(mat, clusters)
  if (is_twomode(object)) {
    out[["modes"]] <- 2
    dims <- dim(as_matrix(object))
    nodes1 <- 1:dims[1]
    nodes2 <- (dims[1] + 1):ncol(out$blocked.data)
    out[["blocked.data"]] <- out$blocked.data[nodes1,nodes2]
    memb <- out$block.membership
    out[["block.membership"]] <- NULL
    out[["block.membership"]][["nodes1"]] <- memb[nodes1]
    out[["block.membership"]][["nodes2"]] <- memb[nodes2] - max(memb[nodes1])
    out[["plabels"]] <- NULL
    rownames(out[["blocked.data"]]) <- out[["plabels"]][["nodes1"]] <- rownames(as_matrix(object))
    colnames(out[["blocked.data"]]) <- out[["plabels"]][["nodes2"]] <- colnames(as_matrix(object))
    orders <- out[["order.vector"]]
    out[["order.vector"]] <- NULL
    out[["order.vector"]][["nodes1"]] <- orders[nodes1]
    out[["order.vector"]][["nodes2"]] <- orders[nodes2] - max(orders[nodes1])
  } else {
    out[["modes"]] <- 1
    rownames(out[["blocked.data"]]) <- out[["plabels"]] <- rownames(mat)
    colnames(out[["blocked.data"]]) <- out[["glabels"]] <- colnames(mat)
  }
  out
}

#' @rdname blockmodel
#' @param x An object of class "block_model"
#' @param ... Additional arguments passed to generic print method
#' @export
print.block_model <- function(x, ...) {
  if (is.null(x$modes)) {
    sna::print.blockmodel(x)
  } else if (x$modes == 1) {
    cat("\nNetwork Blockmodel:\n\n")
    cat("Block membership:\n\n")
    if (is.null(x$plabels)) {
      plab <- seq_len(x$block.membership)[x$order.vector]
    } else if (is.list(x$plabels)) {
      plab <- x$plabels[[1]]
    } else {
      plab <- x$plabels
    }
    temp <- matrix(x$block.membership, nrow = 1)
    dimnames(temp) <- list("", plab)
    print(temp[1, order(x$order.vector)])
  } else {
    cat("\nNetwork Blockmodel:\n\n")
    cat("Block membership:\n\n")
    cat("  First nodeset:\n\n")
    if (is.null(x$plabels)) {
      plab <- seq_len(x$block.membership$nodes1)[x$order.vector$nodes1]
    } else {
      plab <- x$plabels[[1]]
    } 
    temp <- matrix(x$block.membership$nodes1, nrow = 1)
    dimnames(temp) <- list("", plab)
    print(temp[1, order(x$order.vector$nodes1)], ...)
    cat("\n  Second nodeset:\n\n")
    if (is.null(x$plabels)) {
      plab <- seq_len(x$block.membership$nodes2)[x$order.vector$nodes2]
    } else {
      plab <- x$plabels[[2]]
    } 
    temp <- matrix(x$block.membership$nodes2, nrow = 1)
    dimnames(temp) <- list("", plab)
    print(temp[1, order(x$order.vector$nodes2)], ...)
  }
  cat("\nReduced form blockmodel:\n\n")
  if (length(dim(x$block.model)) > 2) {
    for (i in 1:dim(x$block.model)[1]) {
      temp <- x$block.model[i, , ]
      dimnames(temp) <- list(x$rlabels, x$rlabels)
      cat("\t", x$glabels[i], "\n")
      print(temp, ...)
      cat("\n")
    }
  }
  else {
    temp <- x$block.model
    dimnames(temp) <- list(x$rlabels, x$rlabels)
    cat("\t", x$glabels, "\n")
    print(temp, ...)
  }
}

#' @describeIn  blockmodel creates an igraph object from the blockmodel output
#' @export
reduce_graph <- function(blockmodel, block_labels = NULL){
  reduced <- igraph::graph_from_adjacency_matrix(blockmodel$block.model,
                                                 weighted = T)
  reduced <- igraph::delete_edges(reduced,
                                  which(is.nan(igraph::E(reduced)$weight)))
  if (!is.null(block_labels)) igraph::V(reduced)$name <- block_labels
  reduced
}

#' ggplot2-based plotting of blockmodel results
#' @name blockmodel_vis
#' @param x A blockmodel-class object.
#' @param ... Additional arguments passed on to ggplot2.
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot geom_tile aes scale_fill_gradient theme_grey labs theme scale_x_discrete scale_y_discrete geom_vline geom_hline element_blank element_text
#' @importFrom rlang .data
#' @examples
#' # Unsigned
#' usa_concor <- blockmodel_concor(mpn_elite_usa_advice)
#' plot(usa_concor)
#' 
#' # Signed network (Needs sign)
#' # marvel_concor <- blockmodel_concor(ison_marvel_relationships)
#' # plot(marvel)
#' @export
plot.block_model <- function(x, ...) {
  plot_data <- x[["blocked.data"]]
  plot_data <- as.data.frame(plot_data) %>%
    tibble::rownames_to_column("Var1") %>%
    tidyr::pivot_longer(!.data[["Var1"]], names_to = "Var2", values_to = "value")
  g <- ggplot2::ggplot(plot_data, ggplot2::aes(.data[["Var2"]], .data[["Var1"]])) +
    ggplot2::theme_grey(base_size = 9) +
    ggplot2::labs(x = "", y = "") +
    ggplot2::theme(
      legend.position = "none",
      axis.ticks = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(
        size = 9 * 0.8,
        colour = "grey50"
      ),
      axis.text.x = ggplot2::element_text(
        size = 9 * 0.8,
        angle = 30, hjust = 0,
        colour = "grey50"
      )
    )
  # Determine whether the network is signed e.g. only has -1, 0, 1
  if (all(c(-1, 0, 1) %in% x[["blocked.data"]])) {
    # Is the network unimodal?
    if (x[["modes"]] == 1) {
      g <- g +
        ggplot2::geom_tile(ggplot2::aes(fill = as.factor(.data[["value"]])),
                           colour = "white"
        ) +
        # Color for signed networks
        ggplot2::scale_fill_manual(name = "Side", values = c(
          "#003049",
          "white",
          "#d62828"
        )) +
        ggplot2::scale_x_discrete(
          expand = c(0, 0),
          position = "top",
          limits = colnames(x[["blocked.data"]])[x[["order.vector"]]]
        ) +
        ggplot2::scale_y_discrete(
          expand = c(0, 0),
          limits = rev(rownames(x[["blocked.data"]])[x[["order.vector"]]])
        ) +
        ggplot2::geom_vline(
          xintercept = c(1 + which(diff(x$block.membership) != 0))
          - .5,
          colour = "red"
        ) +
        ggplot2::geom_hline(
          yintercept = nrow(x[["blocked.data"]]) -
            c(1 + which(diff(x[["block.membership"]]) != 0)) +
            1.5,
          colour = "red"
        )
    } else {
      g <- g +
        ggplot2::geom_tile(ggplot2::aes(fill = as.factor(.data[["value"]])),
                           colour = "white"
        ) +
        # Color palette for signed networks
        ggplot2::scale_fill_manual(name = "Side", values = c(
          "#003049",
          "white",
          "#d62828"
        )) +
        ggplot2::scale_y_discrete(
          expand = c(0, 0),
          limits = rev(rownames(x[["blocked.data"]])[x[["order.vector"]][["nodes1"]]])
        ) +
        ggplot2::scale_x_discrete(
          expand = c(0, 0),
          position = "top",
          limits = colnames(x[["blocked.data"]])[x[["order.vector"]][["nodes2"]]]
        ) +
        ggplot2::geom_vline(
          xintercept =
            c(1 + which(diff(x[["block.membership"]][["nodes2"]]) != 0))
          - .5,
          colour = "blue"
        ) +
        ggplot2::geom_hline(
          yintercept = nrow(x[["blocked.data"]])
          - c(1 + which(diff(x[["block.membership"]][["nodes1"]]) != 0))
          + 1.5,
          colour = "red"
        )
    }
  } else {
    if (x[["modes"]] == 1) {
      g <- g +
        ggplot2::geom_tile(ggplot2::aes(fill = .data[["value"]]),
                           colour = "white"
        ) +
        ggplot2::scale_fill_gradient(
          low = "white",
          high = "black"
        ) +
        ggplot2::scale_x_discrete(
          expand = c(0, 0),
          position = "top",
          limits = colnames(x[["blocked.data"]])[x[["order.vector"]]]
        ) +
        ggplot2::scale_y_discrete(
          expand = c(0, 0),
          limits = rev(rownames(x[["blocked.data"]])[x[["order.vector"]]])
        ) +
        ggplot2::geom_vline(
          xintercept = c(1 + which(diff(x$block.membership) != 0))
          - .5,
          colour = "red"
        ) +
        ggplot2::geom_hline(
          yintercept = nrow(x[["blocked.data"]]) -
            c(1 + which(diff(x[["block.membership"]]) != 0)) +
            1.5,
          colour = "red"
        )
    } else {
      g <- g +
        ggplot2::geom_tile(ggplot2::aes(fill = .data[["value"]]),
                           colour = "white"
        ) +
        ggplot2::scale_fill_gradient(
          low = "white",
          high = "black"
        ) +
        ggplot2::scale_y_discrete(
          expand = c(0, 0),
          limits = rev(rownames(x[["blocked.data"]])[x[["order.vector"]][["nodes1"]]])
        ) +
        ggplot2::scale_x_discrete(
          expand = c(0, 0),
          position = "top",
          limits = colnames(x[["blocked.data"]])[x[["order.vector"]][["nodes2"]]]
        ) +
        ggplot2::geom_vline(
          xintercept =
            c(1 + which(diff(x[["block.membership"]][["nodes2"]]) != 0))
          - .5,
          colour = "blue"
        ) +
        ggplot2::geom_hline(
          yintercept = nrow(x[["blocked.data"]])
          - c(1 + which(diff(x[["block.membership"]][["nodes1"]]) != 0))
          + 1.5,
          colour = "red"
        )
    }
  }
  g
}

elementwise.all.equal <- Vectorize(function(x, y) {isTRUE(all.equal(x, y))})


