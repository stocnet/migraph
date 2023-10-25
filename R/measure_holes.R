#' Measures of structural holes
#' 
#' @description
#'   These function provide different measures of the degree to which nodes
#'   fill structural holes, as outlined in Burt (1992).
#'   Burt's theory holds that while those nodes embedded in dense clusters
#'   of close connections are likely exposed to the same or similar ideas and information,
#'   those who fill structural holes between two otherwise disconnected groups
#'   can gain some comparative advantage from that position.
#' @details
#'   A number of different ways of measuring these structural holes are available.
#'   Note that we use Borgatti's reformulation for unweighted networks in
#'   `node_redundancy()` and `node_effsize()`.
#'   Redundancy is thus \eqn{\frac{2t}{n}}, 
#'   where \eqn{t} is the sum of ties and \eqn{n} the sum of nodes in each node's neighbourhood,
#'   and effective size is calculated as \eqn{n - \frac{2t}{n}}.
#'   Node efficiency is the node's effective size divided by its degree.
#' @name holes
#' @family measures
#' @references 
#' Burt, Ronald S. 1992. 
#' _Structural Holes: The Social Structure of Competition_. 
#' Cambridge, MA: Harvard University Press.
#' @inheritParams is
NULL

#' @describeIn holes Returns the sum of bridges to which each node
#'   is adjacent.
#' @examples 
#' node_bridges(ison_adolescents)
#' node_bridges(ison_southern_women)
#' @export
node_bridges <- function(.data){
  g <- manynet::as_igraph(.data)
  .inc <- NULL
  out <- vapply(igraph::V(g), function(ego){
    length(igraph::E(g)[.inc(ego) & tie_is_bridge(g)==1])
  }, FUN.VALUE = numeric(1))
  make_node_measure(out, .data)
}

#' @describeIn holes Returns a measure of the redundancy of each nodes'
#'   contacts.
#' @references 
#' Borgatti, Steven. 1997. 
#' “\href{http://www.analytictech.com/connections/v20(1)/holes.htm}{Structural Holes: Unpacking Burt’s Redundancy Measures}” 
#' _Connections_ 20(1):35-38.
#' @examples 
#' node_redundancy(ison_adolescents)
#' node_redundancy(ison_southern_women)
#' @export
node_redundancy <- function(.data){
  g <- manynet::as_igraph(.data)
  .inc <- NULL
  out <- vapply(igraph::V(g), function(ego){
    n = igraph::neighbors(g, ego)
    t = length(igraph::E(g)[.inc(n) & !.inc(ego)])
    n = length(n)
    2 * t / n
  }, FUN.VALUE = numeric(1))
  make_node_measure(out, .data)
}

#' @describeIn holes Returns nodes' effective size
#' @examples 
#' node_effsize(ison_adolescents)
#' node_effsize(ison_southern_women)
#' @export
node_effsize <- function(.data){
  g <- manynet::as_igraph(.data)
  .inc <- NULL
  out <- vapply(igraph::V(g), function(ego){
    n = igraph::neighbors(g, ego)
    t = length(igraph::E(g)[.inc(n) & !.inc(ego)])
    n = length(n)
    n - 2 * t / n
  }, FUN.VALUE = numeric(1))
  make_node_measure(out, .data)
}

#' @describeIn holes Returns nodes' efficiency
#' @examples 
#' node_efficiency(ison_adolescents)
#' node_efficiency(ison_southern_women)
#' @export
node_efficiency <- function(.data){
  out <- node_effsize(.data) / node_degree(.data, normalized = FALSE)
  make_node_measure(as.numeric(out), .data)
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
node_constraint <- function(.data) {
  if (manynet::is_twomode(.data)) {
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
    inst.res <- get_constraint_scores(manynet::as_matrix(.data))
    actr.res <- get_constraint_scores(t(manynet::as_matrix(.data)))
    res <- c(actr.res, inst.res)
  } else {
    res <- igraph::constraint(manynet::as_igraph(.data), 
                              nodes = igraph::V(.data), 
                              weights = NULL)
  }
  res <- make_node_measure(res, .data)
  res
}

#' @describeIn holes Returns nodes' exposure to hierarchy,
#'   where only one or two contacts are the source of closure
#' @examples 
#' node_hierarchy(ison_adolescents)
#' node_hierarchy(ison_southern_women)
#' @export
node_hierarchy <- function(.data){
  cs <- node_constraint(.data)
  g <- manynet::as_igraph(.data)
  out <- vapply(igraph::V(g), function(ego){
    n = igraph::neighbors(g, ego)
    N <- length(n)
    css <- cs[n]
    CN <- mean(css)
    rj <- css/CN
    sum(rj*log(rj)) / (N * log(N))
  }, FUN.VALUE = numeric(1))
  out[is.nan(out)] <- 0
  make_node_measure(out, .data)
}

#' @describeIn holes Returns nodes' eccentricity or Koenig number,
#'   a measure of farness based on number of links needed to reach 
#'   most distant node in the network
#' @importFrom igraph eccentricity
#' @export
node_eccentricity <- function(.data){
  out <- igraph::eccentricity(manynet::as_igraph(.data),
                              mode = "out")
  make_node_measure(out, .data)
}

#' @describeIn holes Returns nodes' average nearest neighbors degree,
#'   or knn,
#'   a measure of the type of local environment a node finds itself in
#' @importFrom igraph knn
#' @references
#' Barrat, Alain, Marc Barthelemy, Romualdo Pastor-Satorras, and Alessandro Vespignani. 2004.
#' "The architecture of complex weighted networks",
#' _Proc. Natl. Acad. Sci._ 101: 3747.
#' @export
node_neighbours_degree <- function(.data){
  out <- igraph::knn(manynet::as_igraph(.data),
                              mode = "out")$knn
  make_node_measure(out, .data)
}

#' @describeIn holes Returns the ratio between common neighbors to ties'
#'   adjacent nodes and the total number of adjacent nodes,
#'   where high values indicate ties' embeddedness in dense local environments
#' @export
tie_cohesion <- function(.data){
  ties <- igraph::E(.data)
  coins <- data.frame(heads = igraph::head_of(.data, ties),
                      tails = igraph::tail_of(.data, ties))
  out <- apply(coins, 1, 
        function(x){
          neigh1 <- igraph::neighbors(.data, x[1])
          neigh2 <- igraph::neighbors(.data, x[2])
          shared_nodes <- sum(c(neigh1 %in% neigh2, 
                                neigh2 %in% neigh1))/2
          neigh_nodes <- length(unique(c(neigh1, neigh2)))-2
          shared_nodes / neigh_nodes
        } )
  make_node_measure(out, .data)
}
