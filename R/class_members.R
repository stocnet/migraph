make_node_member <- function(out, .data) {
  if(is.numeric(out))
    out <- MORELETTERS[out]
  if (manynet::is_labelled(.data)) names(out) <- manynet::node_names(.data)
  class(out) <- c("node_member", class(out))
  attr(out, "mode") <- manynet::node_mode(.data)
  out
}

MORELETTERS <- c(LETTERS, sapply(LETTERS, function(x) paste0(x, LETTERS)))

#' @export
print.node_member <- function(x, ..., n = NULL) {
  cat(pillar::style_subtle(paste(length(unique(x)), "groups\n")))
  if (any(attr(x, "mode"))) {
    for(m in c(FALSE, TRUE)){
      suppressWarnings(print_tblvec(y = x[attr(x, "mode") == m], 
                   names = list(names(x)[attr(x, "mode") == m]),
                   n = n))
      if(!m) cat("\n")
    }
  } else {
    suppressWarnings(print_tblvec(y = x, 
                 names = list(names(x)),
                 n = n))
  }
}

#' @export
summary.node_member <- function(object, ...,
                               n = 6,
                               digits = 3) {
  if (any(attr(object, "mode"))) {
    for (i in names(table(object))) {
      if (i == names(table(object))[1]) cat(i, "\n")
      else cat("\n", i, "\n")
      if (!is.null(names(object))) {
        y <- paste(names(object[object == i & attr(object, "mode")]), collapse = ", ")
        z <- paste(names(object[object == i & !attr(object, "mode")]), collapse = ", ")
      } else {
        y <- paste(which(object == i & attr(object, "mode")), collapse = ", ")
        z <- paste(which(object == i & !attr(object, "mode")), collapse = ", ")
      }
      cat("  ", y, "\n")
      cat("  ", z)
    }
  } else {
    for (i in names(table(object))) {
      cat(pillar::style_subtle(paste0("Class ", i, ":")))
      if (!is.null(names(object)))
        y <- paste(names(object[object == i]), collapse = ", ")
      else
        y <- paste(which(object == i), collapse = ", ")
      cat(" ", y)
      if (i != names(table(object))[length(table(object))]) cat("\n")
    }
  }
}

#' @importFrom stats cutree
#' @export
plot.node_member <- function(x, ...) {
  thisRequires("ggdendro")
    hc <- attr(x, "hc")
    k <- attr(x, "k")
    memb <- x[hc$order]
    clust <- memb[!duplicated(memb)]
    colors <- ifelse(match(memb, clust) %% 2,
                     "#000000", "#E20020")
    ggdendro::ggdendrogram(hc, rotate = TRUE) +
      ggplot2::geom_hline(yintercept = hc$height[length(hc$order) - k],
                          linetype = 2,
                          color = "#E20020") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(colour = "#5c666f"),
                     axis.text.y = suppressWarnings(
                       ggplot2::element_text(colour = colors)))
}

# plot(as_matrix(ison_adolescents),
#   membership = node_regular_equivalence(ison_adolescents, "e"))
# plot(as_matrix(ison_southern_women),
#   membership = node_regular_equivalence(ison_southern_women, "e"))
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot geom_tile aes scale_fill_gradient theme_grey labs theme scale_x_discrete scale_y_discrete geom_vline geom_hline element_blank element_text
#' @importFrom rlang .data
#' @export
plot.matrix <- function(x, ..., membership = NULL) {

  if (!manynet::is_twomode(x)) {
    blocked_data <- manynet::as_matrix(x)
    if (!is.null(membership)) blocked_data <- blocked_data[order(membership),
                                                          order(membership)]
  } else if (manynet::is_twomode(x) &&
     length(intersect(membership[!manynet::node_mode(x)], 
                      membership[!manynet::node_mode(x)])) > 0) {
    blocked_data <- manynet::as_matrix(manynet::to_multilevel(x))
    if (!is.null(membership)) blocked_data <- blocked_data[order(membership),
                                                          order(membership)]
  } else {
    blocked_data <- manynet::as_matrix(x)
  }

  plot_data <- as.data.frame(blocked_data) %>%
    dplyr::mutate(Var1 = rownames(blocked_data)) %>%
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
    ) +
    ggplot2::geom_tile(ggplot2::aes(fill = .data[["value"]]),
                       colour = "white"
    )

  # Color for signed networks
  if (manynet::is_signed(x)) {
    g <- g +
      ggplot2::scale_fill_gradient2(high = "#003049",
        mid = "white",
        low = "#d62828")
  } else {
    g <- g +
      ggplot2::scale_fill_gradient(
        low = "white",
        high = "black"
      )
  }

  # Structure for multimodal networks
  if (!manynet::is_twomode(x)) {
    g <- g +
      ggplot2::scale_x_discrete(expand = c(0, 0), position = "top",
                                limits = colnames(blocked_data)
      ) +
      ggplot2::scale_y_discrete(expand = c(0, 0),
                                limits = rev(rownames(blocked_data))
      )
    if (!is.null(membership))
      g <- g + ggplot2::geom_vline(
        xintercept = c(1 + which(diff(membership[order(membership)]) != 0))
        - .5,
        colour = "red"
      ) +
      ggplot2::geom_hline(
        yintercept = nrow(blocked_data) -
          c(1 + which(diff(membership[order(membership)]) != 0)) +
          1.5,
        colour = "red"
      )
  } else {
    g <- g +
      ggplot2::scale_y_discrete(expand = c(0, 0),
                                limits = rev(rownames(x[["blocked.data"]])[x[["order.vector"]][["nodes1"]]])
      ) +
      ggplot2::scale_x_discrete(expand = c(0, 0), position = "top",
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
  g
}

elementwise.all.equal <- Vectorize(function(x, y) {isTRUE(all.equal(x, y))})
