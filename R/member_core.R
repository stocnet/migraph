#' Core-periphery clustering algorithms
#' @description 
#'   This function is used to identify which nodes should belong to the core,
#'   and which to the periphery.
#'   It seeks to minimize the following quantity:
#'   \deqn{Z(S_1) = \sum_{(i<j)\in S_1} \textbf{I}_{\{A_{ij}=0\}} + \sum_{(i<j)\notin S_1} \textbf{I}_{\{A_{ij}=1\}}}
#'   where nodes \eqn{\{i,j,...,n\}} are ordered in descending degree,
#'   \eqn{A} is the adjacency matrix,
#'   and the indicator function is 1 if the predicate is true or 0 otherwise.
#'   Note that minimising this quantity maximises density in the core block
#'   and minimises density in the periphery block;
#'   it ignores ties between these blocks.
#' @inheritParams is
#' @name core-periphery
#' @family memberships
#' @references
#' Borgatti, Stephen P., & Everett, Martin G. 1999. 
#' Models of core /periphery structures. 
#' _Social Networks_, 21, 375–395. 
#' \doi{10.1016/S0378-8733(99)00019-2}
#' 
#' Lip, Sean Z. W. 2011. 
#' “A Fast Algorithm for the Discrete Core/Periphery Bipartitioning Problem.”
#' \doi{10.48550/arXiv.1102.5511}
#' @examples 
#' mpn_elite_usa_advice %>% as_tidygraph %>% 
#'   mutate(corep = node_core(mpn_elite_usa_advice)) %>% 
#'   autographr(node_color = "corep")
#' network_core(mpn_elite_usa_advice)
#' @export
node_core <- function(.data){
  if(manynet::is_directed(.data)) warning("Asymmetric core-periphery not yet implemented.")
  if(manynet::is_weighted(.data)) warning("Weighted core-periphery not yet implemented.")
  degi <- node_degree(.data, normalized = FALSE)
  nord <- order(degi, decreasing = TRUE)
  zbest <- manynet::network_nodes(.data)*3
  kbest <- 0
  z <- 1/2*sum(degi)
  for(k in 1:(manynet::network_nodes(.data)-1)){
    z <- z + k - 1 - degi[nord][k]
    if(z < zbest){
      zbest <- z
      kbest <- k
    }
  }
  out <- ifelse(seq_len(manynet::network_nodes(.data)) %in% nord[seq_len(kbest)],
         1,2)
  if(manynet::is_labelled(.data)) names(out) <- manynet::node_names(.data)
  make_node_member(out, .data)
}