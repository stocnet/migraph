#' Graph theoretic dimensions of hierarchy
#' @inheritParams is
#' @name hierarchy
#' @family measures
#' @references
#' Krackhardt, David. 1994. 
#' Graph theoretical dimensions of informal organizations. 
#' In Carley and Prietula (eds) _Computational Organizational Theory_, 
#' Hillsdale, NJ: Lawrence Erlbaum Associates. Pp. 89-111. 
#' 
#' Everett, Martin, and David Krackhardt. 2012. 
#' “A second look at Krackhardt's graph theoretical dimensions of informal organizations.”
#' _Social Networks_, 34: 159-163.
#' \doi{10.1016/j.socnet.2011.10.006}
#' @examples 
#' network_connectedness(ison_networkers)
#' 1 - network_reciprocity(ison_networkers)
#' network_efficiency(ison_networkers)
#' network_upperbound(ison_networkers)
NULL

#' @describeIn hierarchy Returns the proportion of dyads in the network
#'   that are reachable, otherwise degree to which network is a single component
#' @export
network_connectedness <- function(.data){
  dists <- igraph::distances(manynet::as_igraph(.data))
  make_network_measure(1 - sum(dists==Inf)/sum(dists!=0),
                       .data)
}

#' @describeIn hierarchy Returns the Krackhardt efficiency score
#' @export
network_efficiency <- function(.data) {
  degs <- node_indegree(.data, normalized = FALSE)
  out <- (manynet::network_nodes(.data)-1)/sum(degs)
  make_network_measure(out, .data)
}

#' @describeIn hierarchy Returns the Krackhardt (least) upper bound score
#' @export
network_upperbound <- function(.data) {
  dists <- igraph::distances(.data, mode = "in")
  dists[is.infinite(dists)] <- 0
  dists <- dists[order(rowSums(dists)), order(rowSums(dists))]
  if (max(colSums(dists > 0)) / (manynet::network_nodes(.data)-1) == 1){
    out <- 1
  } else {
    out <- apply(utils::combn(2:nrow(dists), 2), 2, 
                 function(x){
                   ubs <- dists[x,]>0
                   any(ubs[1,]*ubs[2,]==1)
                 })
    out <- sum(out)/length(out)
  }
  make_network_measure(out, .data)
}