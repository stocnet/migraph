#' Constraint for one- and two-mode networks
#' 
#' This function measures constraint for both one-mode and two-mode networks.
#' For one-mode networks, the function wraps the implementation of Ron Burt's
#' measure in `{igraph}`.
#' For two-mode networks, the function employs the extension outlined
#' in Hollway et al. (2020).
#' @param object A matrix, igraph graph, or tidygraph object.
#' @param nodes The vertices for which the constraint will be calculated. 
#' Defaults to all vertices.
#' @param weights The weights of the edges.
#' If this is NULL and there is a weight edge attribute this is used.
#' If there is no such edge attribute all edges will have the same weight.
#' @return A named vector (one-mode) or a list of two named vectors (`$nodes1`, `$nodes2`).
#' @references Hollway, James, Jean-Frédéric Morin, and Joost Pauwelyn. 2020.
#' \href{https://link.springer.com/article/10.1007/s10784-019-09464-5}{"Structural conditions for novelty: the introduction of new environmental clauses to the trade regime complex."}
#' \emph{International Environmental Agreements: Politics, Law and Economics} 20 (1): 61–83.
#' @family one-mode measures
#' @family two-mode measures
#' @family node-level measures
#' @examples
#' node_constraint(southern_women)
#' @export 
node_constraint <- function(object, nodes = V(object), weights = NULL) {
  if (is_twomode(object)) {
    get_constraint_scores <- function(mat) {
      inst <- colnames(mat)
      rowp <- mat * matrix(1 / rowSums(mat), nrow(mat), ncol(mat))
      colp <- mat * matrix(1 / colSums(mat), nrow(mat), ncol(mat), byrow = T)
      res <- vector()
      for (i in inst) {
        ci <- 0
        membs <- names(which(mat[, i] == 1))
        for (a in membs) {
          pia <- colp[a, i]
          oth <- membs[membs != a]
          pbj <- 0
          if (length(oth) == 1) {
            for (j in inst[mat[oth, ] > 0 & inst != i]) {
              pbj <- sum(pbj, sum(colp[oth, i] * rowp[oth, j] * colp[a, j]))
            }
          } else {
            for (j in inst[colSums(mat[oth, ]) > 0 & inst != i]) {
              pbj <- sum(pbj, sum(colp[oth, i] * rowp[oth, j] * colp[a, j]))
            }
          }
          cia <- (pia + pbj)^2
          ci <- sum(ci, cia)
        }
        res <- c(res, ci)
      }
      names(res) <- inst
      res
    }
    inst.res <- get_constraint_scores(as_matrix(object))
    actr.res <- get_constraint_scores(t(as_matrix(object)))
    res <- list(nodes1 = actr.res, nodes2 = inst.res)

  } else {
    object <- as_igraph(object)
    res <- igraph::constraint(object, nodes = nodes, weights = weights)
  }
  res
}
