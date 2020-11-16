#' Degree centrality for two-mode networks
#'
#' This function substitutes tidygraph::centrality_degree()
#' with a version that correctly normalizes for two-mode networks.
#' @param mat A matrix
#' @family two-mode functions
#' @export
#' @examples
#' centrality_degree()
centrality_degree <- function (weights = NULL, mode = "out", loops = TRUE, normalized = FALSE){
  tidygraph:::expect_nodes()
  graph <- .G()
  weights <- enquo(weights)
  weights <- rlang::eval_tidy(weights, .E())
  if (is.null(weights)) {
    weights <- NA
  }
  if (is_bipartite(graph) & normalized){
    degrees <- degree(graph = graph, v = V(graph), mode = mode, loops = loops)
    other_set_size <- ifelse(V(graph)$type, sum(!V(graph)$type), sum(V(graph)$type))
    degrees/other_set_size
  } else {
    if (is.null(weights)) {
      degree(graph = graph, v = V(graph), mode = mode, loops = loops, 
             normalized = normalized)
    }
    else {
      strength(graph = graph, vids = V(graph), mode = mode, 
               loops = loops, weights = weights)
    }
  }
}
