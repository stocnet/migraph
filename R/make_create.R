#' Create networks with particular structures
#' 
#' @description 
#'   These functions create networks with particular structural properties.
#'   They can create either one-mode or two-mode networks,
#'   depending on whether the common `n` argument
#'   is passed a single integer (the number of nodes in the one-mode network)
#'   or a vector of \emph{two} integers to return a two-mode network
#'   (the first integer indicates the number of nodes in the first mode,
#'   the second integer indicates the number of nodes in the second mode).
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
#'   By default FALSE. 
#'   If the opposite direction is desired, use `to_redirected()`.
#' @param width Either an integer specifying the width or breadth
#'   of the ring or branches,
#'   or a proportion indicating how many nodes in a mode should
#'   be part of the core.
#' @param ... Additional arguments passed on to igraph.
#' @return By default an igraph object will be returned,
#'   but this can be coerced into other types of objects
#'   using `as_matrix()`, `as_tidygraph()`, or `as_network()`.
#' @importFrom tidygraph as_tbl_graph
#' @importFrom igraph graph_from_incidence_matrix
NULL

infer_n <- function(n){
  if(is_migraph(n)) n <- graph_dims(n)
  if(length(n)>2) stop(paste("`n` should be a single integer for a one-mode network or", 
                             "a vector of two integers for a two-mode network."))
  n
}

infer_membership <- function(n, membership){
  if(is.null(membership)){
    if(length(n)>1){
      membership <- c(sort(abs(seq_len(n[1]) %% 2 -2)), 
                      sort(abs(seq_len(n[2]) %% 2 -2)))
    } else membership <- sort(abs(seq_len(n) %% 2 -2))
  }
  membership
}

#' @describeIn create Creates an empty graph of the given dimensions.
#' @examples
#' autographr(create_empty(10)) + autographr(create_complete(10))
#' autographr(create_empty(c(8,6))) + autographr(create_complete(c(8,6)))
#' @export
create_empty <- function(n) {
  n <- infer_n(n)
  if (length(n) == 1) {
    out <- matrix(0, n, n)
    out <- igraph::graph_from_adjacency_matrix(out)
  } else if (length(n) == 2) {
    out <- matrix(0, n[1], n[2])
    out <- igraph::graph_from_incidence_matrix(out)
  } 
  out
}

#' @describeIn create Creates a filled graph of the given dimensions,
#'   with every possible tie realised. 
#' @export
create_complete <- function(n, directed = FALSE) {
  n <- infer_n(n)
  if (length(n) == 1) {
    out <- matrix(1, n, n)
    diag(out) <- 0
    out <- igraph::graph_from_adjacency_matrix(out,
                                               ifelse(directed, 
                                                      "directed", "undirected"))
  } else if (length(n) == 2) {
    out <- matrix(1, n[1], n[2])
    out <- igraph::graph_from_incidence_matrix(out)
  }
}

#' @describeIn create Creates a ring or chord graph of the given dimensions
#' that loops around is of a certain width or thickness.
#' @examples
#' autographr(create_ring(8, width = 2)) + 
#' autographr(create_ring(c(8,6), width = 2))
#' @export
create_ring <- function(n, width = 1, directed = FALSE, ...) {
  n <- infer_n(n)
  
  # Helper function
  roll_over <- function(w) {
    cbind(w[, ncol(w)], w[, 1:(ncol(w) - 1)])
  }
  
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
      if (directed) {
        out <- igraph::graph_from_adjacency_matrix(out, mode = "directed")
      } else out <- igraph::graph_from_adjacency_matrix(out, mode = "undirected")
    }
  } else if (length(n) == 2) {
    n1 <- n[1]
    n2 <- n[2]
    mat <- matrix(0, n1, n2)
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
    if (width != 0) mat <- mat + roll_over(mat)
    if (width > 1) {
      for (i in 1:(width - 1)) {
        w <- roll_over(mat)
        mat <- mat + w
      }
    }
    out <- igraph::graph_from_incidence_matrix(mat)
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
    out <- igraph::graph_from_incidence_matrix(out)
  }
  out
}

#' @describeIn create Creates a graph of the given dimensions with successive branches.
#' @importFrom igraph make_tree
#' @examples
#' autographr(create_tree(15, directed = TRUE)) + 
#' autographr(create_tree(15, directed = TRUE), "tree") + 
#' autographr(create_tree(15, directed = TRUE, width = 3), "tree")
#' @export
create_tree <- function(n, 
                        directed = FALSE, 
                        width = 2) {
  n <- infer_n(n)
  if(length(n)>1) stop("`create_tree()` not yet implemented for two-mode networks")
  igraph::make_tree(sum(n), children = width, 
                    mode = ifelse(directed, "out", "undirected"))
}

#' @describeIn create Creates a graph of the given dimensions with ties to all neighbouring nodes
#' @importFrom igraph make_lattice
#' @examples
#' autographr(create_lattice(5), layout = "kk") +
#' autographr(create_lattice(c(5,5))) +
#' autographr(create_lattice(c(5,5,5)))
#' @export
create_lattice <- function(n, 
                           directed = FALSE) {
  n <- infer_n(n)
  igraph::make_lattice(n, directed = directed)
}

#' @describeIn create Creates a graph in which the nodes are clustered
#'   into separate components.
#' @examples
#' autographr(create_components(10, membership = c(1,1,1,2,2,2,3,3,3,3)))
#' autographr(create_components(c(10, 12)))
#' @export
create_components <- function(n, membership = NULL) {
  n <- infer_n(n)
  membership <- infer_membership(n, membership)
  if (length(n) == 1) {
    out <- matrix(0, n, n)
    for(x in unique(membership)) out[membership == x, membership == x] <- 1
    diag(out) <- 0
    out <- as_igraph(out)
  } else if (length(n) == 2) {
    out <- matrix(0, n[1], n[2])
    for(x in unique(membership)) out[membership[1:n[1]] == x, 
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
create_core <- function(n, membership = NULL){
  n <- infer_n(n)
  membership <- infer_membership(n, membership)
  if(length(n)>1){
    mat <- matrix(0, n[1], n[2])
    mat[membership[1:n[1]]==1,] <- 1
    mat[,membership[(n[1]+1):length(membership)]==1] <- 1
    as_igraph(mat, twomode = TRUE)
  } else {
    mat <- matrix(0, n, n)
    mat[membership==1,] <- 1
    mat[,membership==1] <- 1
    diag(mat) <- 0
    as_igraph(mat)
  }
}

# #' @rdname create
#' #' @details Creates a nested two-mode network.
#' #' Will construct an affiliation matrix,
#' #' with decreasing fill across n2.
#' #' @importFrom tidygraph as_tbl_graph
#' #' @importFrom igraph graph_from_incidence_matrix
#' #' @examples
#' #' create_nest(10, 12)
#' #' @export
#' create_nest <- function(n1, n2,
#'                         as = c("tidygraph", "igraph", "matrix")) {
#' 
#'   as <- match.arg(as)
#' 
#'   out <- matrix(0, n1, n2)
#'   out[(row(out) - col(out)) >= 0] <- 1
#' 
#'   if(as == "tidygraph") out <- tidygraph::as_tbl_graph(out)
#'   if(as == "igraph") out <- igraph::graph_from_incidence_matrix(out)
#'   out
#' }
#' 
#' # mat.dist <- matrix(0,5,3)
#' # mat.dist[1:2,1] <- 1
#' # mat.dist[,2] <- 1
#' # mat.dist[4:5,3] <- 1
#' #
#' # # mat.part <- matrix(0,5,5)
#' # # mat.part[1:3,1] <- 1
#' # # mat.part[1:2,2] <- 1
#' # # mat.part[4:5,3] <- 1
#' # # mat.part[4:5,4] <- 1
#' # # mat.part[3,5] <- 1
#' # #
#' # mat.part <- mat.dist
#' # mat.part[2,1] <- 0
#' # mat.part[1,2] <- 0
#' # mat.part[4,3] <- 0
#' #
#' # mat.side <- matrix(0,4,4)
#' # mat.side[1:4,1] <- 1
#' # mat.side[1,2] <- 1
#' # mat.side[2,3] <- 1
#' # mat.side[3,4] <- 1
#' #
#' # mat.core <- matrix(0,4,4)
#' # mat.core[1:4,1] <- 1
#' # mat.core[1:2,2] <- 1
#' # mat.core[3:4,3] <- 1
#' # mat.core[1:2,4] <- 1
#' #
#' # mat.hier <- matrix(0,4,4)
#' # mat.hier[1:4,1] <- 1
#' # mat.hier[1:2,2] <- 1
#' # mat.hier[1:2,3] <- 1
#' # mat.hier[3:4,4] <- 1

