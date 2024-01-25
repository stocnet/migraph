#' Core-periphery clustering algorithms
#' @description
#'   These functions identify nodes belonging to (some level of) the core of a network:
#'   
#'   - `node_core()` assigns nodes to either the core or periphery.
#'   - `node_coreness()` assigns nodes to their level of k-coreness.
#' 
#' @inheritParams cohesion
#' @param method Which method to use to identify cores and periphery.
#'   By default this is "degree", 
#'   which relies on the heuristic that high degree nodes are more likely to be in the core.
#'   An alternative is "eigenvector", which instead begins with high eigenvector nodes.
#'   Other methods, such as a genetic algorithm, CONCOR, and Rombach-Porter,
#'   can be added if there is interest.
#' @name core
#' @family memberships
NULL

#' @rdname core
#' @section Core-periphery: 
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
#' #mpn_elite_usa_advice %>% as_tidygraph %>% 
#' #   mutate(corep = node_core(mpn_elite_usa_advice)) %>% 
#' #   autographr(node_color = "corep")
#' network_core(mpn_elite_usa_advice)
#' @export
node_core <- function(.data, method = c("degree", "eigenvector")){
  method <- match.arg(method)
  if(manynet::is_directed(.data)) warning("Asymmetric core-periphery not yet implemented.")
  if(method == "degree"){
    degi <- node_degree(.data, normalized = FALSE, 
                        alpha = ifelse(manynet::is_weighted(.data), 1, 0))
  } else if (method == "eigenvector") {
    degi <- node_eigenvector(.data, normalized = FALSE)
  } else stop("This function expects either 'degree' or 'eigenvector' method to be specified.")
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
  make_node_member(out, .data)
}

#' @rdname core
#' @examples
#' node_coreness(ison_adolescents)
#' @export
node_coreness <- function(.data){
  if(!manynet::is_graph(.data)) .data <- manynet::as_igraph(.data)
  out <- igraph::coreness(.data)
  make_node_member(out, .data)
}

