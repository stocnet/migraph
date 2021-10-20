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
#' @examples
#' ggraphgrid(mpn_elite_mex)
#' ggraphgrid(mpn_elite_usa_advice)
#' ggraphgrid(mpn_elite_usa_money)
#' ggraphgrid(mpn_ryanair)
#' @export
ggraphgrid <- function(x, algorithm = c("kk", "fr")) {
  name <- NULL # initialize variables to avoid CMD check notes
  x <- as_tidygraph(x)
  algorithm <- match.arg(algorithm)

  gg <- ggraph::create_layout(x, layout = "igraph", 
                              algorithm = algorithm, maxiter = 10000)
  ggraph::ggraph(x, graph = depth_first_recursive_search(gg)) +
    ggraph::geom_conn_bundle(data = ggraph::get_con(
                              from = igraph::as_edgelist(x, names = FALSE)[, 1],
                              to = igraph::as_edgelist(x, names = FALSE)[, 2]),
                             alpha = 0.1) +
    # ggraph::geom_edge_link(arrow = arrow(length = unit(3, 'mm')),
    #                        start_cap = circle(4, 'mm'),
    #                        end_cap = circle(4, 'mm'), show.legend = FALSE) +
  ggraph::geom_node_text(aes(label = ifelse(is.null(name), "", name))) + ggplot2::theme_void()
}

depth_first_recursive_search <- function(layout) {

  dims <- ceiling(2 * sqrt(nrow(layout)))
  vacant_points <- expand.grid(0:dims, 0:dims)
  vacant_points <- vacant_points - floor(dims / 2)
  names(vacant_points) <- c("x", "y")
  layout <- layout[order(abs(layout$x) + abs(layout$y)), ]
  for (i in seq_len(nrow(layout))) {
    dists <- as.matrix(dist(rbind(layout[i, 1:2], vacant_points),
                            method = "manhattan"))[, 1]
    mindist <- which(dists == min(dists[2:length(dists)]))[1] - 1
    vacpoint <- vacant_points[mindist, ]
    changes <- vacpoint - layout[i, 1:2]
    layout[i:nrow(layout), 1] <- layout[i:nrow(layout), 1] + changes[[1]]
    layout[i:nrow(layout), 2] <- layout[i:nrow(layout), 2] + changes[[2]]
    vacant_points <- vacant_points[-mindist, ]
  }
  layout[order(layout$.ggraph.index), ]
}

localmin <- function(layout, graph) {
  repeat {
    f0 <- sum(cost_function(layout, graph))
    L <- get_vacant_points(layout)
    for (a in seq_len(nrow(layout))) {
      out <- t(apply(L, 1, function(y) {
        layout_new <- layout
        layout_new[a, 1:2] <- y
        c(a, y, sum(cost_function(layout_new, graph)))
      }))
    }
    if (out[which.min(out[, 4]), 4] < f0) {
      layout[out[which.min(out[, 4]), 1], 1:2] <- out[which.min(out[, 4]), 2:3]
    } else{
      break
    }
  }
  layout
}

get_vacant_points <- function(layout) {
  all_points <- expand.grid(min(layout$x):max(layout$x),
                            min(layout$y):max(layout$y))
  names(all_points) <- c("x", "y")
  vacant_points <- rbind(all_points,
                         layout[, c("x", "y")])
  vacant_points <- subset(vacant_points,
                          !(duplicated(vacant_points) |
                              duplicated(vacant_points, fromLast = TRUE)))
  vacant_points
}

cost_function <- 
  function(layout, graph, max_repulse_distance = max(layout[, 1]) * .75) {
  d <- as.matrix(dist(layout[, 1:2], method = "manhattan"))
  a <- as_matrix(graph)
  i <- diag(nrow(a))
  m <- a + i
  w <- ifelse(m > 0, 3,
              ifelse(m == 0 & m %*% t(m) > 0, 0, -2)) # only three levels here
  # see Li and Kurata (2005: 2037) for more granulated option
  ifelse(w >= 0, w * d, w * min(d, max_repulse_distance))
}

plot_gl <- function(x, tmax, tmin, rmin, fmin, ne, rc, p) {

  l <- NULL # initialize variables to avoid CMD check notes
  index <- NULL # initialize variables to avoid CMD check notes
  a <- NULL # initialize variables to avoid CMD check notes

  x <- as_tidygraph(x)
  lo <- ggraph::create_layout(x, layout = "igraph", algorithm = "randomly")
  lo[, 1] <- round(lo[, 1] * 1000)
  lo[, 2] <- round(lo[, 2] * 1000)
  dists <- as.matrix(dist(lo[, 1:2], method = "manhattan"))
  colMax <- function(data) apply(data, MARGIN = 1, FUN = max, na.rm = TRUE)
  diag(dists) <- NA
  rsep <- l * sum(ifelse(colMax(a / dists - 1) > 0, colMax(a / dists - 1), 0))
  ggraph::ggraph(x, graph = lo) +
    geom_edge_link(aes(alpha = stat(index)), show.legend = FALSE) +
    geom_node_point()
}
