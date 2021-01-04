#' Two-mode chain
#'
#' Creates a two-mode chain
#' @param n1 Number of nodes in the first node set
#' @param n2 Number of nodes in the second node set
#' @param as What type of object to return.
#' One of "matrix", "tbl_graph", "igraph".
#' By default, creates a "tbl_graph" object.
#' @details Will construct a bilateral lattice,
#' with two ties for every second-mode node.
#' @importFrom tidygraph as_tbl_graph
#' @importFrom igraph graph_from_incidence_matrix
#' @export
#' @examples
#' \dontrun{
#' create_chain(5,10) %>% ggraph() +
#' geom_edge_fan(aes(alpha = stat(index)), show.legend = FALSE) +
#' geom_node_point(aes(size = 5))
#' }
create_chain <- function(n1, n2, 
                         as = c("tbl_graph", "igraph", "matrix")) {
  
  mat <- matrix(0, n1, n2)
  out <- suppressWarnings(((row(mat) - col(mat)) == 0 |
    (row(mat) - col(mat)) == (-seq.int(0, n2 - 1, n1)[-1]) |
    (row(mat) - col(mat)) == -1 |
    (row(mat) - col(mat)) == -seq.int(1 + n1, n2 - 1, n1) |
    (row(mat) - col(mat)) == nrow(mat) - 1) * 1)
  
  as <- match.arg(as)
  if(as == "tbl_graph") out <- tidygraph::as_tbl_graph(out)
  if(as == "igraph") out <- igraph::graph_from_incidence_matrix(out)
  out
}

#' Matched two-mode graph
#'
#' Creates a matched two-mode network
#' @param n1 Number of nodes in the first node set
#' @param n2 Number of nodes in the second node set
#' @param as What type of object to return.
#' @details Will construct an affiliation matrix,
#' with by default both n1 and n2 matched.
#' TODO: Incorporate into create_chain (chordal_ring of certain breadth w).
#' @importFrom tidygraph as_tbl_graph
#' @importFrom igraph graph_from_incidence_matrix
#' @export
#' @examples
#' \dontrun{
#' create_match(10, 12)
#' }
create_match <- function(n1, n2,
                         as = c("tbl_graph", "igraph", "matrix")) {
  mat <- matrix(0, n1, n2)
  mat <- matrix(
    ((row(mat) - col(mat)) %in% unique(-seq(0, n2 - 1, n1), 
                                       seq(0, n1 - 1, n2))) * 1,
    n1, n2
  )

  as <- match.arg(as)
  if(as == "tbl_graph") mat <- tidygraph::as_tbl_graph(mat)
  if(as == "igraph") mat <- igraph::graph_from_incidence_matrix(mat)
  mat
}

#' Two-mode random graph
#'
#' Creates a random two-mode network
#' @param n1 Number of nodes in the first node set
#' @param n2 Number of nodes in the second node set
#' @param p Number of edges in the network over the number of edges possible
#' @param m Number of edges in the network
#' @param as What type of object to return.
#' @param mode How should edges be followed
#' @param directed Should direction of edges be used for the calculations
#' One of "matrix", "tbl_graph", "igraph".
#' By default, creates a "tbl_graph" object.
#' @details Will construct an affiliation matrix,
#' with a random probability of a tie.
#' @importFrom tidygraph play_bipartite
#' @importFrom igraph as.igraph as_adjacency_matrix
#' @export
#' @examples
#' \dontrun{
#' play_twomode(10, 12, 0.25) %>% ggraph() +
#' geom_edge_fan(aes(alpha = stat(index)), show.legend = FALSE) +
#' geom_node_point(aes(size = 5))
#' }
play_twomode <- function(n1, n2, p, m, directed = TRUE, mode = "out",
                           as = c("tbl_graph", "igraph", "matrix")) {
  
  g <- tidygraph::play_bipartite(n1, n2, p, m, directed, mode)

  as <- match.arg(as)
  if(as == "igraph" | as == "matrix") g <- igraph::as.igraph(g)
  if(as == "matrix") g <- igraph::as_adjacency_matrix(g)
  g
}

#' Two-component two-mode graph
#'
#' Creates a two-component two-mode network
#' @param n1 Number of nodes in the first node set
#' @param n2 Number of nodes in the second node set
#' @param as What type of object to return.
#' @details Will construct an affiliation matrix,
#' with full component diagonal.
#' TODO: Allow specfication of how many silos/components to create
#' @importFrom tidygraph as_tbl_graph
#' @importFrom igraph graph_from_incidence_matrix
#' @export
#' @examples
#' \dontrun{
#' create_silos(10, 12)
#' }
create_silos <- function(n1, n2,
                         as = c("tbl_graph", "igraph", "matrix")) {
  mat <- matrix(0, n1, n2)
  mat[1:round(n1 / 2), 1:round(n2 / 2)] <- 1
  mat[rowSums(mat) < 1, colSums(mat) < 1] <- 1

  as <- match.arg(as)
  if(as == "tbl_graph") mat <- tidygraph::as_tbl_graph(mat)
  if(as == "igraph") mat <- igraph::graph_from_incidence_matrix(mat)
  mat
}

#' Nested two-mode graph
#'
#' Creates a nested two-mode network
#' @param n1 Number of nodes in the first node set
#' @param n2 Number of nodes in the second node set
#' @param as What type of object to return.
#' @details Will construct an affiliation matrix,
#' with decreasing fill across n2.
#' @importFrom tidygraph as_tbl_graph
#' @importFrom igraph graph_from_incidence_matrix
#' @export
#' @examples
#' \dontrun{
#' create_nest(10, 12)
#' }
create_nest <- function(n1, n2, 
                        as = c("tbl_graph", "igraph", "matrix")) {
  
  as <- match.arg(as)
  
  out <- matrix(0, n1, n2)
  out[(row(out) - col(out)) >= 0] <- 1

  if(as == "tbl_graph") out <- tidygraph::as_tbl_graph(out)
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
