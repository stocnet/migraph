# Grid layouts ####

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

# Partition layouts ####

#' Layout algorithms based on bi- or other partitions
#' @name partition_layouts
#' @inheritParams transform
#' @inheritParams grid_layouts
#' @param radius A vector of radii at which the concentric circles
#'   should be located.
#'   By default this is equal placement around an empty centre, 
#'   unless one (the core) is a single node,
#'   in which case this node occupies the centre of the graph.
#' @param order.by An attribute label indicating the (decreasing) order
#'   for the nodes around the circles. 
#'   By default ordering is given by a bipartite placement that reduces
#'   the number of edge crossings.
#' @family mapping
#' @source
#' Diego Diez, Andrew P. Hutchins and Diego Miranda-Saavedra. 2014.
#' "Systematic identification of transcriptional regulatory modules from
#' protein-protein interaction networks". 
#' _Nucleic Acids Research_, 42 (1) e6.
#' @importFrom BiocManager install
#' @examples 
#' (autographr(ison_southern_women, "hierarchy") /
#' autographr(ison_southern_women, "railway")) |
#' autographr(ison_southern_women, "concentric")
#' autographr(ison_karateka, "hierarchy") 
#' @export
layout_tbl_graph_hierarchy <- function(object,
                                       circular = FALSE, times = 1000){
  
  if (!requireNamespace("Rgraphviz", quietly = TRUE)){
    BiocManager::install("Rgraphviz")
  }
  
  prep <- as_matrix(object, twomode = FALSE)
  if(anyDuplicated(rownames(prep))){
    rownames(prep) <- seq_len(nrow(prep))
    colnames(prep) <- seq_len(ncol(prep))
  }
  if(any(prep<0)) prep[prep<0] <- 0
  out <- as_graphAM(prep)
  out <- suppressMessages(Rgraphviz::layoutGraph(out, layoutType = 'dot', 
                                                 attrs = list(graph = list(rankdir = "BT"))))
  nodeX <- .rescale(out@renderInfo@nodes$nodeX)
  nodeY <- .rescale(out@renderInfo@nodes$nodeY)
  # nodeY <- abs(nodeY - max(nodeY))
  .to_lo(cbind(nodeX, nodeY))
}

#' @rdname partition_layouts
#' @export
layout_tbl_graph_alluvial <- function(object,
                                      circular = FALSE, times = 1000){
  if (!requireNamespace("Rgraphviz", quietly = TRUE)){
    BiocManager::install("Rgraphviz")
  }
  
  prep <- as_matrix(object, twomode = FALSE)
  if(anyDuplicated(rownames(prep))){
    rownames(prep) <- seq_len(nrow(prep))
    colnames(prep) <- seq_len(ncol(prep))
  }
  if(any(prep<0)) prep[prep<0] <- 0
  out <- as_graphAM(prep)
  out <- suppressMessages(Rgraphviz::layoutGraph(out, layoutType = 'dot', 
                                                 attrs = list(graph = list(rankdir = "LR"))))
  nodeX <- .rescale(out@renderInfo@nodes$nodeX)
  nodeY <- .rescale(out@renderInfo@nodes$nodeY)
  # nodeY <- abs(nodeY - max(nodeY))
  .to_lo(cbind(nodeX, nodeY))  
}

#' @rdname partition_layouts
#' @export
layout_tbl_graph_railway <- function(object,
                                     circular = FALSE, times = 1000){
  res <- layout_tbl_graph_hierarchy(as_igraph(object))
  res$x <- c(match(res[res[,2]==0,1], sort(res[res[,2]==0,1])),
             match(res[res[,2]==1,1], sort(res[res[,2]==1,1])))
  res
}

#' @rdname partition_layouts
#' @export
layout_tbl_graph_ladder <- function(object,
                                    circular = FALSE, times = 1000){
  res <- layout_tbl_graph_alluvial(as_igraph(object))
  res$y <- c(match(res[res[,2]==1,1], sort(res[res[,2]==1,1])),
             match(res[res[,2]==0,1], sort(res[res[,2]==0,1])))
  res
}

#' @rdname partition_layouts
#' @export
layout_tbl_graph_concentric <- function(object, membership = NULL, radius = NULL, 
                                        order.by = NULL, 
                                        circular = FALSE, times = 1000){
  if (is.null(membership)){
    if(!is_twomode(object)) 
      membership <- to_list(node_core(object))
    else membership <- to_list(node_mode(object))
  }
  all_c  <- unlist(membership, use.names = FALSE)
  if (any(table(all_c) > 1)) 
    stop("Duplicated nodes in layers!")
  if(is_labelled(object))
    all_n <- node_names(object) else
      all_n <- 1:network_nodes(object)
  sel_other  <- all_n[!all_n %in% all_c]
  if (length(sel_other) > 0) 
    membership[[length(membership) + 1]] <- sel_other
  if (is.null(radius)) {
    radius <- seq(0, 1, 1/(length(membership)))
    if (length(membership[[1]]) == 1) 
      radius <- radius[-length(radius)] else 
        radius <- radius[-1]
  }
  if (!is.null(order.by)){
    order.values <- lapply(order.by, 
                           function(b) node_attribute(object, b))
  } else {
    for(k in 2:length(membership)){
      xnet <- as_matrix(to_multilevel(object))[membership[[k-1]], 
                                               membership[[k]]]
      lo <- layout_tbl_graph_hierarchy(as_igraph(xnet, twomode = TRUE))
      lo$names <- node_names(object)
      if(ncol(lo)==2) lo[,1] <- seq_len(lo)
      order.values <- lapply(1:0, function(x)
        if(ncol(lo)>=3) sort(lo[lo[,2]==x,])[,3] 
        else sort(lo[lo[,2]==x,1]) ) 
    }
    # order.values <- getNNvec(object, members)
  }
  res <- matrix(NA, nrow = length(all_n), ncol = 2)
  for (k in seq_along(membership)) {
    r <- radius[k]
    l <- order.values[[k]]
    if(is_labelled(object))
      l <- match(l, node_names(object))
    res[l, ] <- getCoordinates(l, r)
  }
  .to_lo(res)
}

.rescale <- function(vector){
  (vector - min(vector)) / (max(vector) - min(vector))
}

.to_lo <- function(mat){
  res <- as.data.frame(mat)
  names(res) <- c("x","y")
  res
}

to_list <- function(members){
  out <- lapply(sort(unique(members)), function(x){
    y <- which(members==x)
    if(!is.null(names(y))) names(y) else y
  })
  names(out) <- unique(members)
  out
}

getNNvec <- function(object, members){
  lapply(members, function(circle){
    diss <- 1 - cor(to_multilevel(as_matrix(object))[, circle])
    diag(diss) <- NA
    if(is_labelled(object))
      starts <- names(sort(node_degree(object)[circle], decreasing = TRUE)[1])
    else starts <- paste0("V",1:network_nodes(object))[sort(node_degree(object)[circle], 
                                                            decreasing = TRUE)[1]]
    if(length(circle)>1)
      starts <- c(starts, names(which.min(diss[starts,])))
    out <- starts
    if(length(circle)>2){
      for(i in 1:(length(circle)-2)){
        diss <- diss[,!colnames(diss) %in% starts]
        if(is.matrix(diss)){
          side <- names(which.min(apply(diss[starts,], 1, min, na.rm = TRUE)))
          new <- names(which.min(diss[side,]))
        } else {
          side <- names(which.min(diss[starts]))
          new <- setdiff(circle,out)
        }
        if(side == out[1]){
          out <- c(new, out)
          starts <- c(new, starts[2])
        } else {
          out <- c(out, new)
          starts <- c(starts[1], new)
        }
      }
    }
    out
  })
}

getCoordinates <- function(x, r){
  l <- length(x)
  d <- 360/l
  c1 <- seq(0, 360, d)
  c1 <- c1[1:(length(c1) - 1)]
  tmp <- t(vapply(c1, 
                  function(cc) c(cos(cc * pi/180) * 
                                   r, sin(cc *
                                            pi/180) * r),
                  FUN.VALUE = numeric(2)))
  rownames(tmp) <- x
  tmp
}

