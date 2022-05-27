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


