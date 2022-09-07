#' Layouts for snapping layouts to a grid
#'
#' @description The function uses approximate pattern matching
#'   to redistribute coarse layouts on square grid points, while
#'   preserving the topological relationships among the nodes (see Inoue et al. 2012). 
#' @inheritParams is
#' @param circular Should the layout be transformed into a radial representation. 
#' Only possible for some layouts. Defaults to FALSE
#' @param times Maximum number of iterations, where appropriate
#' @importFrom ggraph create_layout ggraph geom_edge_link geom_node_text
#' @importFrom igraph as_edgelist
#' @importFrom stats dist
#' @references
#' Inoue, Kentaro, Shinichi Shimozono, Hideaki Yoshida, and Hiroyuki Kurata. 2012. 
#' “Application of Approximate Pattern Matching in Two Dimensional Spaces to Grid Layout for Biochemical Network Maps” edited by J. Bourdon. 
#' _PLoS ONE_ 7(6):e37739.
#' \doi{https://doi.org/10.1371/journal.pone.0037739}.
#' @name grid_layouts
#' @family mapping
NULL

#' @rdname grid_layouts
#' @export
layout_tbl_graph_frgrid <- function(object, circular = FALSE, times = 1000){
  xy <- as.data.frame(igraph::layout_with_fr(object, maxiter = times))
  colnames(xy) <- c("x","y")
  xy <- depth_first_recursive_search(xy)
  attr(xy, 'graph') <- as_tidygraph(object)
  xy
}

#' @rdname grid_layouts
#' @export
layout_tbl_graph_kkgrid <- function(object, circular = FALSE, times = 1000){
  xy <- as.data.frame(igraph::layout_with_kk(object, maxiter = times))
  colnames(xy) <- c("x","y")
  xy <- depth_first_recursive_search(xy)
  attr(xy, 'graph') <- as_tidygraph(object)
  xy
}

#' @rdname grid_layouts
#' @export
layout_tbl_graph_gogrid <- function(object, circular = FALSE, times = 1000){
  xy <- as.data.frame(igraph::layout_with_graphopt(object, niter = times))
  colnames(xy) <- c("x","y")
  xy <- depth_first_recursive_search(xy)
  attr(xy, 'graph') <- as_tidygraph(object)
  xy
}

#' @rdname grid_layouts
#' @export
layout_tbl_graph_stressgrid <- function(object, circular = FALSE, times = 1000){
  xy <- as.data.frame(ggraph::create_layout(object, "stress")[,1:2])
  colnames(xy) <- c("x","y")
  xy <- depth_first_recursive_search(xy)
  attr(xy, 'graph') <- as_tidygraph(object)
  xy
}

depth_first_recursive_search <- function(layout) {
  
  dims <- ceiling(2 * sqrt(nrow(layout)))
  vacant_points <- expand.grid(0:dims, 0:dims)
  vacant_points <- vacant_points - floor(dims / 2)
  names(vacant_points) <- c("x", "y")
  layout <- layout[order(abs(layout[,1]) + abs(layout[,2])), ]
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
  layout
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

cost_function <- function(layout, graph, max_repulse_distance = max(layout[, 1]) * .75) {
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
  l <- index <- a <- NULL # initialize variables to avoid CMD check notes
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
