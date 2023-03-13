#' Make networks with defined structures
#' 
#' @description 
#'   These functions create networks with particular structural properties.
#'   They can create either one-mode or two-mode networks.
#'   To create a one-mode network, pass the main argument `n` a single integer,
#'   indicating the number of nodes in the network.
#'   To create a two-mode network, pass `n` a vector of \emph{two} integers,
#'   where the first integer indicates the number of nodes in the first mode,
#'   and the second integer indicates the number of nodes in the second mode.
#'   As an alternative, an existing network can be provided to `n`
#'   and the number of modes, nodes, and directedness will be inferred.
#'   
#'   By default, all networks are created as undirected.
#'   This can be overruled with the argument `directed = TRUE`.
#'   This will return a directed network in which the arcs are
#'   out-facing or equivalent.
#'   This direction can be swapped using `to_redirected()`.
#'   In two-mode networks, this is ignored.
#' @name create
#' @family makes
#' @seealso [as]
#' @param n Given:
#'   \itemize{
#'   \item A single integer, e.g. `n = 10`,
#'   a one-mode network will be created.
#'   \item A vector of two integers, e.g. `n = c(5,10)`,
#'   a two-mode network will be created.
#'   \item A migraph-compatible object,
#'   a network of the same dimensions will be created.
#'   }
#' @param directed Logical whether the graph should be directed.
#'   By default `directed = FALSE`.
#'   If the opposite direction is desired, use `to_redirected()`.
#' @param width Integer specifying the width of the ring,
#'   breadth of the branches, or maximum extent of the neighbourbood.
#' @param membership A vector of partition membership as integers.
#'   If left as `NULL` (the default), nodes in each mode will be
#'   assigned to two, equally sized partitions.
#' @param ... Additional arguments passed on to `{igraph}`.
#' @return By default an `igraph` object is returned,
#'   but this can be coerced into other types of objects
#'   using `as_edgelist()`, `as_matrix()`,
#'   `as_tidygraph()`, or `as_network()`.
#' @importFrom tidygraph as_tbl_graph
#' @importFrom igraph graph_from_incidence_matrix
NULL

#' @describeIn create Creates an empty graph of the given dimensions.
#' @examples
#' autographr(create_empty(10)) + autographr(create_filled(10))
#' autographr(create_empty(c(8,6))) + autographr(create_filled(c(8,6)))
#' @export
create_empty <- function(n, directed = FALSE) {
  directed <- infer_directed(n, directed)
  n <- infer_n(n)
  if (length(n) == 1) {
    out <- matrix(0, n, n)
    out <- igraph::graph_from_adjacency_matrix(out)
  } else if (length(n) == 2) {
    out <- matrix(0, n[1], n[2])
    out <- as_igraph(out, twomode = TRUE)
  }
  if(!directed) out <- to_undirected(out)
  out
}

#' @describeIn create Creates a complete graph of the given dimensions,
#'   with every possible tie realised. 
#' @export
create_filled <- function(n, directed = FALSE) {
  directed <- infer_directed(n, directed)
  n <- infer_n(n)
  if (length(n) == 1) {
    out <- matrix(1, n, n)
    diag(out) <- 0
    out <- igraph::graph_from_adjacency_matrix(out,
                                               ifelse(directed, 
                                                      "directed", "undirected"))
  } else if (length(n) == 2) {
    out <- matrix(1, n[1], n[2])
    out <- as_igraph(out, twomode = TRUE)
  }
}

#' @describeIn create Creates a ring or chord graph of the given dimensions
#' that loops around is of a certain width or thickness.
#' @examples
#' autographr(create_ring(8, width = 2)) +
#' autographr(create_ring(c(8,6), width = 2))
#' @export
create_ring <- function(n, directed = FALSE, width = 1, ...) {
  directed <- infer_directed(n, directed)
  n <- infer_n(n)
  if (length(n) == 1) {
    if (width == 1) {
     out <- igraph::make_ring(n, directed, ...)
    } else {
      out <- w <- as_matrix(igraph::make_ring(n, directed, ...))
      for (i in 1:(width - 1)) {
        w <- roll_over(w)
        out <- out + w
      }
      diag(out) <- 0
      out[out > 1] <- 1
      out <- igraph::graph_from_adjacency_matrix(out,
                                                 ifelse(directed, 
                                                        "directed", "undirected"))
    }
  } else if (length(n) == 2) {
    mat <- matrix(0, n[1], n[2])
    diag(mat) <- 1
    while (any(rowSums(mat) == 0)) {
      top <- mat[rowSums(mat) == 1, ]
      bot <- mat[rowSums(mat) == 0, ]
      diag(bot) <- 1
      mat <- rbind(top, bot)
    }
    while (any(colSums(mat) == 0)) {
      left <- mat[, colSums(mat) == 1]
      right <- mat[, colSums(mat) == 0]
      diag(right) <- 1
      mat <- cbind(left, right)
    }
    for (i in 1:(width)) {
      w <- roll_over(mat)
      mat <- mat + w
    }
    mat[mat > 1] <- 1
    out <- as_igraph(mat, twomode = TRUE)
  }
  out
}

#' @describeIn create Creates a graph of the given dimensions
#'   that has a maximally central node
#' @importFrom igraph graph_from_adjacency_matrix graph_from_incidence_matrix make_star
#' @examples
#' autographr(create_star(12)) +
#' autographr(create_star(12, directed = TRUE)) +
#' autographr(create_star(c(12,1)))
#' @export
create_star <- function(n,
                        directed = FALSE) {
  directed <- infer_directed(n, directed)
  n <- infer_n(n)
  if (length(n) == 1) {
    out <- igraph::make_star(n,
                             mode = ifelse(directed, "out", "undirected"))
  } else if (length(n) == 2) {
    out <- matrix(0, n[1], n[2])
    if (directed) {
      out[1, ] <- 1
    } else {
      out[, 1] <- 1
    }
    out <- as_igraph(out, twomode = TRUE)
  }
  out
}

#' @describeIn create Creates a graph of the given dimensions with successive branches.
#' @importFrom igraph make_tree
#' @examples
#' autographr(create_tree(c(7,8), directed = TRUE)) +
#' autographr(create_tree(15, directed = TRUE), "tree") +
#' autographr(create_tree(15, directed = TRUE, width = 3), "tree")
#' @export
create_tree <- function(n,
                        directed = FALSE,
                        width = 2) {
  directed <- infer_directed(n, directed)
  n <- infer_n(n)
  if (length(n) > 1) {
    out <- matrix(0, n[1], n[2])
    avail1 <- 1:n[1]
    avail2 <- 1:n[2]
    on1 <- 1
    avail1 <- avail1[avail1 != on1]
    while (length(avail1) > 0 & length(avail2) > 0) {
      on2 <- vector()
      for (i in on1) {
        matches <- avail2[1:width]
        out[i, matches] <- 1
        on2 <- c(on2, matches)
        suppressWarnings(avail2 <- avail2[avail2 != matches])
      }
      on1 <- vector()
      for (j in on2) {
        matches <- avail1[1:width]
        out[matches, j] <- 1
        on1 <- c(on1, matches)
        suppressWarnings(avail1 <- avail1[avail1 != matches])
      }
    }
    as_igraph(out, twomode = TRUE)
  } else igraph::make_tree(sum(n), children = width,
                    mode = ifelse(directed, "out", "undirected"))
}

#' @describeIn create Creates a lattice graph of the given dimensions with ties to all neighbouring nodes
#' @section Lattice graphs:
#'   `create_lattice()` creates both two-dimensional grid and triangular lattices with as even dimensions
#'   as possible.
#'   When the `width` parameter is set to 4, nodes cannot have (in or out) degrees larger than 4.
#'   This creates regular square grid lattices where possible.
#'   Such a network is bipartite, that is partitionable into two types that are not adjacent to any
#'   of their own type.
#'   If the number of nodes is a prime number, it will only return a chain (a single dimensional lattice).
#'   
#'   A `width` parameter of 8 creates a network where the maximum degree of any nodes is 8.
#'   This can create a triangular mesh lattice or a Queen's move lattice, depending on the dimensions.
#'   A `width` parameter of 12 creates a network where the maximum degree of any nodes is 12.
#'   Prime numbers of nodes will return a chain.
#' @importFrom igraph make_lattice
#' @examples
#' autographr(create_lattice(12, width = 4), layout = "kk") +
#' autographr(create_lattice(12, width = 8), layout = "kk") +
#' autographr(create_lattice(12, width = 12), layout = "kk")
#' @export
create_lattice <- function(n,
                        directed = FALSE, 
                        width = 8) {
  directed <- infer_directed(n, directed)
  n <- infer_n(n)
  if(length(n)== 1){
    divs <- divisors(n)
    if((length(divs) %% 2) == 0){
      dims <- c(divs[length(divs)/2], divs[length(divs)/2+1])
    } else dims <- c(median(divs), median(divs))
    if(width == 8){
      nei1.5 <- as_matrix(igraph::make_lattice(dims, nei = 2, 
                                               directed = directed))
      for(i in 1:(prod(dims)-2)){
        nei1.5[i,i+2] <- 0
        if(i+dims[1]*2<=prod(dims))
          nei1.5[i,i+dims[1]*2] <- 0
      }
      if(!directed)
        nei1.5[lower.tri(nei1.5)] <- t(nei1.5)[lower.tri(nei1.5)]
      as_igraph(nei1.5)
    } else if (width == 12){
      igraph::make_lattice(dims, nei = 2, directed = directed)
    } else if (width == 4){
      igraph::make_lattice(dims, nei = 1, directed = directed)
    } else stop("`max_neighbourhood` expected to be 4, 8, or 12")
  } else {
    divs1 <- divisors(n[1])
    divs2 <- divisors(n[2])
    # divs1 <- divs1[-c(1, length(divs1))]
    # divs2 <- divs2[-c(1, length(divs2))]
    divs1 <- intersect(divs1, divs2)
    divs2 <- intersect(divs2, divs1)
    # divs1 <- intersect(divs1, c(divs2+1, divs2-1))
    # divs2 <- intersect(divs2, c(divs1+1, divs1-1))
    mat <- matrix(0, n[1], n[2])
    diag(mat) <- 1
    w <- roll_over(mat)
    mat <- mat + w
    mat[lower.tri(mat)] <- 0
    out <- mat[rowSums(mat)==2,]
    out <- do.call(rbind, replicate(nrow(mat)/nrow(out), out, simplify=FALSE))
    as_igraph(out)
  }
}

# #' @describeIn create Creates a honeycomb-style, isometric, or triangular grid/mesh lattice graph 
# #'   of the given dimensions with ties to nodes up to a maximum width.
# #' @importFrom igraph make_lattice
# #' @examples
# #' autographr(create_mesh(5), layout = "kk")
# #' @export
# create_mesh <- function(n,
#                         directed = FALSE, 
#                         width = 8) {
  
  # offset_divisors <- function(x){
  #   y <- seq_len(x)
  #   y[ x%%y == 0 ]
  # }
  # 
  # if(length(n)== 1){
  #   divs <- offset_divisors(n)
    # if((length(divs) %% 2) == 0){
    #   dims <- c(divs[length(divs)/2], divs[length(divs)/2+1])
    # } else dims <- c(median(divs), median(divs))
    # if(width == 8){
    #   nei1.5 <- as_matrix(igraph::make_lattice(dims, nei = 2, 
    #                                            directed = directed))
    #   for(i in 1:(prod(dims)-2)){
    #     nei1.5[i,i+2] <- 0
    #     if(i+dims[1]*2<=prod(dims))
    #       nei1.5[i,i+dims[1]*2] <- 0
    #   }
    #   if(!directed)
    #     nei1.5[lower.tri(nei1.5)] <- t(nei1.5)[lower.tri(nei1.5)]
    #   as_igraph(nei1.5)
    # } else if (width == 12){
    #   igraph::make_lattice(dims, nei = 2, directed = directed)
    # } else if (width == 4){
    #   igraph::make_lattice(dims, nei = 1, directed = directed)
    # } else stop("`max_neighbourhood` expected to be 4, 8, or 12")
#   }
# }
  
#' @describeIn create Creates a graph in which the nodes are clustered
#'   into separate components.
#' @examples
#' autographr(create_components(10, membership = c(1,1,1,2,2,2,3,3,3,3)))
#' autographr(create_components(c(10, 12)))
#' @export
create_components <- function(n, directed = FALSE, membership = NULL) {
  directed <- infer_directed(n, directed)
  membership <- infer_membership(n, membership)
  n <- infer_n(n)
  if (length(n) == 1) {
    out <- matrix(0, n, n)
    for (x in unique(membership)) out[membership == x, membership == x] <- 1
    diag(out) <- 0
    if(directed) out[lower.tri(out)] <- 0
    out <- as_igraph(out)
  } else if (length(n) == 2) {
    out <- matrix(0, n[1], n[2])
    for (x in unique(membership)) out[membership[1:n[1]] == x,
                                     membership[(n[1]+1):length(membership)] == x] <- 1
    out <- as_igraph(out, twomode = TRUE)
  }
  out
}

#' @describeIn create Creates a graph with a certain proportion of nodes
#'   being core nodes, densely tied to each other and peripheral nodes,
#'   and the rest peripheral, tied only to the core.
#' @examples
#' autographr(create_core(6)) +
#' autographr(create_core(6, membership = c(1,1,1,1,2,2))) +
#' autographr(create_core(c(6,6)))
#' @export
create_core <- function(n, directed = FALSE, membership = NULL) {
  directed <- infer_directed(n, directed)
  membership <- infer_membership(n, membership)
  n <- infer_n(n)
  if (length(n) > 1) {
    mat <- matrix(0, n[1], n[2])
    mat[membership[1:n[1]]==1,] <- 1
    mat[, membership[(n[1] + 1):length(membership)]==1] <- 1
    as_igraph(mat, twomode = TRUE)
  } else {
    mat <- matrix(0, n, n)
    mat[membership==1,] <- 1
    mat[, membership==1] <- 1
    diag(mat) <- 0
    if(directed) mat[lower.tri(mat)] <- 0
    as_igraph(mat)
  }
}

# #' @rdname create
# #' @details Creates a nested two-mode network.
# #' Will construct an affiliation matrix,
# #' with decreasing fill across n2.
# #' @importFrom tidygraph as_tbl_graph
# #' @importFrom igraph graph_from_incidence_matrix
# #' @examples
# #' create_nest(10, 12)
# #' @export
# create_nest <- function(n1, n2,
#                         as = c("tidygraph", "igraph", "matrix")) {
# 
#   as <- match.arg(as)
# 
#   out <- matrix(0, n1, n2)
#   out[(row(out) - col(out)) >= 0] <- 1
# 
#   if(as == "tidygraph") out <- tidygraph::as_tbl_graph(out)
#   if(as == "igraph") out <- igraph::graph_from_incidence_matrix(out)
#   out
# }
# 
# # mat.dist <- matrix(0,5,3)
# # mat.dist[1:2,1] <- 1
# # mat.dist[,2] <- 1
# # mat.dist[4:5,3] <- 1
# #
# # mat.hier <- matrix(0,4,4)
# # mat.hier[1:4,1] <- 1
# # mat.hier[1:2,2] <- 1
# # mat.hier[1:2,3] <- 1
# # mat.hier[3:4,4] <- 1

# Helper functions ------------------

infer_n <- function(n) {
  if (is_migraph(n)) n <- network_dims(n)
  if (length(n) > 2) stop(paste("`n` should be a single integer for a one-mode network or",
                             "a vector of two integers for a two-mode network."))
  n
}

infer_directed <- function(n, directed){
  if(is_migraph(n)) directed <- is_directed(n)
  directed
}

infer_membership <- function(n, membership) {
  if (is.null(membership)) {
    if (length(n) > 1) {
      membership <- c(sort(abs(seq_len(n[1]) %% 2 -2)), 
                      sort(abs(seq_len(n[2]) %% 2 -2)))
    } else membership <- sort(abs(seq_len(n) %% 2 -2))
  }
  membership
}

divisors <- function(x){
  y <- seq_len(x)
  y[ x%%y == 0 ]
}

roll_over <- function(w) {
  cbind(w[, ncol(w)], w[, 1:(ncol(w) - 1)])
}
