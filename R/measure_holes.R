#' Structural holes for one- and two-mode networks
#' 
#' @description
#' These function provide different measures of the degree to which nodes
#' fill structural holes, as outlined in Burt (1992).
#' @name holes
#' @family measures
#' @references 
#' #' Burt, Ronald S. 1992. 
#' _Structural Holes: The Social Structure of Competition_. 
#' Cambridge, MA: Harvard University Press.
#' @inheritParams is
NULL

#' @describeIn holes Returns nodes' redundancy
#' @references 
#' Borgatti, Steven. 1997. 
#' “\href{http://www.analytictech.com/connections/v20(1)/holes.htm}{Structural Holes: Unpacking Burt’s Redundancy Measures}” 
#' _Connections_ 20(1):35-38.
#' @examples 
#' node_redundancy(ison_adolescents)
#' node_redundancy(ison_southern_women)
#' @export
node_redundancy <- function(object){
  g <- as_igraph(object)
  .inc <- NULL
  out <- vapply(igraph::V(g), function(ego){
    n = igraph::neighbors(g, ego)
    t = length(igraph::E(g)[.inc(n) & !.inc(ego)])
    n = length(n)
    2 * t / n
  }, FUN.VALUE = numeric(1))
  make_node_measure(out, object)
}

#' @describeIn holes Returns nodes' effective size
#' @examples 
#' node_effsize(ison_adolescents)
#' node_effsize(ison_southern_women)
#' @export
node_effsize <- function(object){
  g <- as_igraph(object)
  .inc <- NULL
  out <- vapply(igraph::V(g), function(ego){
    n = igraph::neighbors(g, ego)
    t = length(igraph::E(g)[.inc(n) & !.inc(ego)])
    n = length(n)
    n - 2 * t / n
  }, FUN.VALUE = numeric(1))
  make_node_measure(out, object)
}

#' @describeIn holes Returns nodes' efficiency
#' @examples 
#' node_efficiency(ison_adolescents)
#' node_efficiency(ison_southern_women)
#' @export
node_efficiency <- function(object){
  out <- node_effsize(object) / node_degree(object, normalized = FALSE)
  make_node_measure(as.numeric(out), object)
}

#' @describeIn holes Returns nodes' constraint scores for one-mode networks
#'   according to Burt (1992) and for two-mode networks according to Hollway et al (2020). 
#' @references
#' Hollway, James, Jean-Frédéric Morin, and Joost Pauwelyn. 2020.
#' "Structural conditions for novelty: the introduction of new environmental clauses to the trade regime complex."
#' _International Environmental Agreements: Politics, Law and Economics_ 20 (1): 61–83.
#' \doi{10.1007/s10784-019-09464-5}.
#' @examples
#' node_constraint(ison_southern_women)
#' @export 
node_constraint <- function(object) {
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
    res <- c(actr.res, inst.res)
  } else {
    res <- igraph::constraint(as_igraph(object), 
                              nodes = igraph::V(object), 
                              weights = NULL)
  }
  res <- make_node_measure(res, object)
  res
}

#' @describeIn holes Returns nodes' exposure to hierarchy,
#'   where only one or two contacts are the source of closure
#' @examples 
#' node_hierarchy(ison_adolescents)
#' node_hierarchy(ison_southern_women)
#' @export
node_hierarchy <- function(object){
  cs <- node_constraint(object)
  g <- as_igraph(object)
  out <- vapply(igraph::V(object), function(ego){
    n = igraph::neighbors(g, ego)
    N <- length(n)
    css <- cs[n]
    CN <- mean(css)
    rj <- css/CN
    sum(rj*log(rj)) / (N * log(N))
  }, FUN.VALUE = numeric(1))
  out[is.nan(out)] <- 0
  make_node_measure(out, object)
}
