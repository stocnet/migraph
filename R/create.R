#' Create networks that conform to particular structures
#' 
#' These functions create a host of different network objects.
#' Despite the common syntax, what distinguishes them from
#' those in other packages is that passing the `n` argument
#' a vector of \emph{two} integers will return a two-mode
#' network instead of a one-mode network.
#' By default an igraph object will be returned,
#' but this can be coerced into other types of objects
#' using `as_matrix()` or `as_tidygraph()`.
#' @name create
#' @family creation
#' @param n1 Number of nodes in the first node set
#' @param n2 Number of nodes in the second node set
#' @param as What type of object to return.
#' One of "matrix", "tidygraph", "igraph".
#' By default, creates tidygraph's "tbl_graph" object.
#' @details Will construct a bilateral lattice,
#' with two ties for every second-mode node.
#' @importFrom tidygraph as_tbl_graph
#' @importFrom igraph graph_from_incidence_matrix
#' @examples
#' create_chain(5,10) %>% ggraph() +
#' geom_edge_fan(aes(alpha = stat(index)), show.legend = FALSE) +
#' geom_node_point(aes(size = 5))
#' @export
create_chain <- function(n1, n2, 
                         as = c("tidygraph", "igraph", "matrix")) {
  
  if(missing(n1)) stop("Need to supply a number of nodes in the first node set")
  if(missing(n2)) stop("Need to supply a number of nodes in the second node set")
  
  mat <- matrix(0, n1, n2)
  out <- suppressWarnings(((row(mat) - col(mat)) == 0 |
    (row(mat) - col(mat)) == (-seq.int(0, n2 - 1, n1)[-1]) |
    (row(mat) - col(mat)) == -1 |
    (row(mat) - col(mat)) == -seq.int(1 + n1, n2 - 1, n1) |
    (row(mat) - col(mat)) == nrow(mat) - 1) * 1)
  
  as <- match.arg(as)
  if(as == "tidygraph") out <- tidygraph::as_tbl_graph(out)
  if(as == "igraph") out <- igraph::graph_from_incidence_matrix(out)
  out
}

#' @rdname create
#' @details Creates a matched two-mode network.
#' Will construct an affiliation matrix,
#' with by default both n1 and n2 matched.
#' TODO: Incorporate into create_chain (chordal_ring of certain breadth w).
#' @importFrom tidygraph as_tbl_graph
#' @importFrom igraph graph_from_incidence_matrix
#' @examples
#' create_match(10, 12)
#' @export
create_match <- function(n1, n2,
                         as = c("tidygraph", "igraph", "matrix")) {
  mat <- matrix(0, n1, n2)
  mat <- matrix(
    ((row(mat) - col(mat)) %in% unique(-seq(0, n2 - 1, n1), 
                                       seq(0, n1 - 1, n2))) * 1,
    n1, n2
  )

  as <- match.arg(as)
  if(as == "tidygraph") mat <- tidygraph::as_tbl_graph(mat)
  if(as == "igraph") mat <- igraph::graph_from_incidence_matrix(mat)
  mat
}

#' @rdname create
#' @details Creates a two-component two-mode network.
#' Will construct an affiliation matrix,
#' with full component diagonal.
#' TODO: Allow specfication of how many silos/components to create
#' @importFrom tidygraph as_tbl_graph
#' @importFrom igraph graph_from_incidence_matrix
#' @examples
#' create_silos(10, 12)
#' @export
create_silos <- function(n1, n2,
                         as = c("tidygraph", "igraph", "matrix")) {
  mat <- matrix(0, n1, n2)
  mat[1:round(n1 / 2), 1:round(n2 / 2)] <- 1
  mat[rowSums(mat) < 1, colSums(mat) < 1] <- 1

  as <- match.arg(as)
  if(as == "tidygraph") mat <- tidygraph::as_tbl_graph(mat)
  if(as == "igraph") mat <- igraph::graph_from_incidence_matrix(mat)
  mat
}

#' @rdname create
#' @details Creates a nested two-mode network.
#' Will construct an affiliation matrix,
#' with decreasing fill across n2.
#' @importFrom tidygraph as_tbl_graph
#' @importFrom igraph graph_from_incidence_matrix
#' @examples
#' create_nest(10, 12)
#' @export
create_nest <- function(n1, n2, 
                        as = c("tidygraph", "igraph", "matrix")) {
  
  as <- match.arg(as)
  
  out <- matrix(0, n1, n2)
  out[(row(out) - col(out)) >= 0] <- 1

  if(as == "tidygraph") out <- tidygraph::as_tbl_graph(out)
  if(as == "igraph") out <- igraph::graph_from_incidence_matrix(out)
  out
}

#' @rdname create
#' @details Will create a complete bipartite start graph
#' @importFrom tidygraph as_tbl_graph
#' @importFrom igraph graph_from_incidence_matrix
#' @examples
#' create_star(1, 12)
#' @export
create_star <- function(n1 = 1, n2,
                        as = c("tidygraph", "igraph", "matrix")){
  as <- match.arg(as)
  
  out <- matrix(0, n1, n2)
  out[1,] <- 1
  
  if(as == "tidygraph") out <- tidygraph::as_tbl_graph(out)
  if(as == "igraph") out <- igraph::graph_from_incidence_matrix(out)
  out
}

#' @rdname create
#' @return `create_complete()` creates a matrix in which all of the
#' cells are filled.
#' @export
create_complete <- function(n,
                            as = c("tidygraph", "igraph", "matrix")){
  
  as <- match.arg(as)
  
  if(length(n)==1){
    out <- matrix(1, n, n)
  } else if (length(n)==2){
    out <- matrix(1, n[1], n[2])
  } else stop("`n` should be a single integer for a one-mode network or a vector of two integers for a two-mode network.")

  if(as == "tidygraph") out <- tidygraph::as_tbl_graph(out)
  if(as == "igraph") out <- igraph::graph_from_incidence_matrix(out)
  out
}

# mat.dist <- matrix(0,5,3)
# mat.dist[1:2,1] <- 1
# mat.dist[,2] <- 1
# mat.dist[4:5,3] <- 1
#
# # mat.part <- matrix(0,5,5)
# # mat.part[1:3,1] <- 1
# # mat.part[1:2,2] <- 1
# # mat.part[4:5,3] <- 1
# # mat.part[4:5,4] <- 1
# # mat.part[3,5] <- 1
# #
# mat.part <- mat.dist
# mat.part[2,1] <- 0
# mat.part[1,2] <- 0
# mat.part[4,3] <- 0
#
# mat.side <- matrix(0,4,4)
# mat.side[1:4,1] <- 1
# mat.side[1,2] <- 1
# mat.side[2,3] <- 1
# mat.side[3,4] <- 1
#
# mat.core <- matrix(0,4,4)
# mat.core[1:4,1] <- 1
# mat.core[1:2,2] <- 1
# mat.core[3:4,3] <- 1
# mat.core[1:2,4] <- 1
#
# mat.hier <- matrix(0,4,4)
# mat.hier[1:4,1] <- 1
# mat.hier[1:2,2] <- 1
# mat.hier[1:2,3] <- 1
# mat.hier[3:4,4] <- 1
