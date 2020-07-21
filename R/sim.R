#' Two-mode lattice
#'
#' Creates a two-mode lattice
#' @param node1 Number of nodes in the first node set
#' @param node2 Number of nodes in the second node set
#' @details Will construct a bilateral lattice,
#' with two ties for every second-mode node.
#' @export
#' @examples
#' \dontrun{
#' create_lattice(10, 12)
#' }
create_lattice <- function(node1, node2) {
  mat <- matrix(0, node1, node2)
  out <- suppressWarnings(((row(mat) - col(mat)) == 0 |
    (row(mat) - col(mat)) == (-seq.int(0, node2 - 1, node1)[-1]) |
    (row(mat) - col(mat)) == -1 |
    (row(mat) - col(mat)) == -seq.int(1 + node1, node2 - 1, node1) |
    (row(mat) - col(mat)) == nrow(mat) - 1) * 1)
  out
}

#' Two-mode random graph
#'
#' Creates a random two-mode network
#' @param node1 Number of nodes in the first node set
#' @param node2 Number of nodes in the second node set
#' @param density Number of edges in the network over the number of edges possible
#' @details Will construct an affiliation matrix,
#' with a random probability of a tie.
#' @export
#' @examples
#' \dontrun{
#' create_random(10, 12, 0.25)
#' }
create_random <- function(node1, node2, density) {
  mat <- matrix(rbinom(node1 * node2, 1, density), node1, node2)
  mat
}

#' Two-component two-mode graph
#'
#' Creates a two-component two-mode network
#' @param node1 Number of nodes in the first node set
#' @param node2 Number of nodes in the second node set
#' @details Will construct an affiliation matrix,
#' with full component diagonal.
#' @export
#' @examples
#' \dontrun{
#' create_poles(10, 12)
#' }
create_poles <- function(node1, node2) {
  mat <- matrix(0, node1, node2)
  mat[1:round(node1 / 2), 1:round(node2 / 2)] <- 1
  mat[rowSums(mat) < 1, colSums(mat) < 1] <- 1
  mat
}

#' Matched two-mode graph
#'
#' Creates a matched two-mode network
#' @param node1 Number of nodes in the first node set
#' @param node2 Number of nodes in the second node set
#' @details Will construct an affiliation matrix,
#' with by default both node1 and node2 matched.
#' TODO: Make node set matching optional.
#' @export
#' @examples
#' \dontrun{
#' create_match(10, 12)
#' }
create_match <- function(node1, node2) {
  mat <- matrix(0, node1, node2)
  mat <- matrix(
    ((row(mat) - col(mat)) %in% unique(-seq(0, node2 - 1, node1), 
                                       seq(0, node1 - 1, node2))) * 1,
    node1, node2
  )
  mat
}

#' Nested two-mode graph
#'
#' Creates a nested two-mode network
#' @param node1 Number of nodes in the first node set
#' @param node2 Number of nodes in the second node set
#' @details Will construct an affiliation matrix,
#' with decreasing fill across node2.
#' TODO: Make node set matching optional.
#' @export
#' @examples
#' \dontrun{
#' create_nest(10, 12)
#' }
create_nest <- function(node1, node2) {
  mat <- matrix(0, node1, node2)
  mat[(row(mat) - col(mat)) >= 0] <- 1
  mat
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
