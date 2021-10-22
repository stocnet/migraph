#' Create networks with particular structures
#' 
#' These functions create a variety of different network objects.
#' Despite the common function names and syntax with existing packages,
#' the common `n` argument can not only be passed
#' a single integer to return a one-mode network,
#' but also a vector of \emph{two} integers to return a two-mode network.
#' 
#' @name create
#' @family creation
#' @param n Number of nodes.
#' If a single integer is given, e.g. `n = 10`,
#' the function will create a one-mode network.
#' If a vector of two integers is given, e.g. `n = c(5,10)`,
#' the function will create a two-mode network.
#' @return By default an igraph object will be returned,
#' but this can be coerced into other types of objects
#' using `as_matrix()`, `as_tidygraph()`, or `as_network()`.
#' @importFrom tidygraph as_tbl_graph
#' @importFrom igraph graph_from_incidence_matrix
#' @seealso as_matrix as_tidygraph as_network
#' @details `create_empty()` creates an empty graph of the given dimensions.
#' @examples
#' g <- create_empty(c(8,6))
#' autographr(g)
#' @export
create_empty <- function(n) {
  if (length(n) == 1) {
    out <- matrix(0, n, n)
    out <- igraph::graph_from_adjacency_matrix(out)
  } else if (length(n) == 2) {
    out <- matrix(0, n[1], n[2])
    out <- igraph::graph_from_incidence_matrix(out)
  } else stop("`n` should be a single integer for a one-mode network or a vector of two integers for a two-mode network.")
  out
}

#' @rdname create
#' @details `create_complete()` creates a filled graph of the given dimensions.
#' @examples
#' g <- create_complete(c(8,6))
#' autographr(g)
#' @export
create_complete <- function(n) {
  
  if (length(n) == 1) {
    out <- matrix(1, n, n)
    diag(out) <- 0
    out <- igraph::graph_from_adjacency_matrix(out)
  } else if (length(n) == 2) {
    out <- matrix(1, n[1], n[2])
    out <- igraph::graph_from_incidence_matrix(out)
  } else stop("`n` should be a single integer for a one-mode network or a vector of two integers for a two-mode network.")
  out
}

#' @rdname create
#' @param width The width or breadth of the ring. This is typically double the degree.
#' @param directed Whether the graph should be directed. By default FALSE.
#' @param ... Additional arguments passed on to igraph.
#' @details `create_ring()` creates a ring or chord graph of the given dimensions
#' that loops around is of a certain width or thickness.
#' @examples
#' g <- create_ring(c(8,6), width = 2)
#' autographr(g)
#' @export
create_ring <- function(n, width = 1, directed = FALSE, ...) {
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
  } else stop("`n` should be a single integer for a one-mode network or a vector of two integers for a two-mode network.")

  out
}

#' @rdname create
#' @param components Number of components to create.
#' @details \code{create_components()} creates a graph in which the nodes are clustered
#' into separate components.
#' @examples
#' autographr(create_components(c(10, 12), components = 3))
#' @export
create_components <- function(n, components = 2) {
  if (length(n) == 1) {
    if (components > n) stop("Cannot have more components than nodes in the graph.")
    out <- matrix(0, n, n)
    for (x in split(1:n, sort(1:n %% components))) {
      out[x, x] <- 1
    }
    diag(out) <- 0
    out <- igraph::graph_from_adjacency_matrix(out)
  } else if (length(n) == 2) {
    if (components > n[1] | components > n[2]) {
      stop("Cannot have more components than nodes in any nodeset.")
    }
    out <- matrix(0, n[1], n[2])
    for (x in 1:components) {
      rows <- split(1:n[1], sort(1:n[1] %% components))[[x]]
      cols <- split(1:n[2], sort(1:n[2] %% components))[[x]]
      out[rows, cols] <- 1
    }
    out <- igraph::graph_from_incidence_matrix(out)
  } else stop("`n` should be a single integer for a one-mode network or a vector of two integers for a two-mode network.")
  out
}

#' @rdname create
#' @param directed One of the following options: "in", "out", or "none".
#' @importFrom igraph graph_from_adjacency_matrix graph_from_incidence_matrix
#' @examples
#' autographr(create_star(c(12,1)))
#' @export
create_star <- function(n, directed = "in") {
  
  if (length(n) == 1) {
    out <- matrix(0, n, n)
    if (directed == "in") {
      out[, 1] <- 1
    } else {
      out[1, ] <- 1
    }
    out <- igraph::graph_from_adjacency_matrix(out)
  } else if (length(n) == 2) {
    out <- matrix(0, n[1], n[2])
    if (directed == "in") {
      out[, 1] <- 1
    } else {
      out[1, ] <- 1
    }
    out <- igraph::graph_from_incidence_matrix(out)
  } else stop("`n` should be a single integer for a one-mode network or a vector of two integers for a two-mode network.")
  out
}
# Helper function
roll_over <- function(w) {
  cbind(w[, ncol(w)], w[, 1:(ncol(w) - 1)])
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
