#' Structural balance
#' @param object a migraph-consistent object
#' @param method one of "triangles" (the default), "walk", or "frustration".
#' @importFrom signnet balance_score
#' @references Estrada, Ernesto (2019). Rethinking structural balance in signed social networks. 
#' _Discrete Applied Mathematics_, 268: 70--90.
#' 
#' Aref, Samin, and Mark C Wilson (2018). Measuring partial balance in signed networks. 
#' _Journal of Complex Networks_, 6(4): 566–595.
#' @return "triangles" returns the proportion of balanced triangles,
#' ranging between `0` if all triangles are imbalanced and `1` if all triangles are balanced.
#' 
#' "walk" is based on eigenvalues of the signed and underlying unsigned network. 
#' Check the paper by Estrada for details. 
#' 
#' “frustration” assumes that the network can be partitioned into two groups, 
#' where intra group edges are positive and inter group edges are negative. 
#' The index is defined as the sum of intra group negative and inter group positive edges. 
#' Note that the problem is NP complete and only an upper bound is returned (based on simulated annealing). 
#' Exact methods can be found in the work of Aref.
#' @export
graph_balance <- function(object, method = "triangles") { 
  signnet::balance_score(as_igraph(object), method)
}
