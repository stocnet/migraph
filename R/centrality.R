#' Degree centrality for two-mode networks
#'
#' This function substitutes tidygraph::centrality_degree()
#' with a version that correctly normalizes for two-mode networks.
#' @param mat A matrix
#' @family two-mode functions
#' @references Borgatti, Stephen P., and Martin G. Everett. "Network analysis of 2-mode data." Social networks 19.3 (1997): 243-270.
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
      degree(graph = graph, V = V(graph), mode = mode, loops = loops, 
             normalized = normalized)
    }
    else {
      strength(graph = graph, vids = V(graph), mode = mode, 
               loops = loops, weights = weights)
    }
  }
}

#' Closeness centrality for two-mode networks
#' 
#' This function substitutes tidygraph::centrality_closeness()
#' with a version that correctly normalizes for two-mode networks.
#'
#' @param weights 
#' @param mode 
#' @param loops 
#' @param normalized 
#' @family two-mode functions
#' @references Borgatti, Stephen P., and Martin G. Everett. "Network analysis of 2-mode data." Social networks 19.3 (1997): 243-270.
#' @return
#'
#' @examples
#' @export
centrality_closeness <- function (weights = NULL, mode = "out", normalized = FALSE, cutoff = NULL){
  tidygraph:::expect_nodes()
  graph <- .G()
  weights <- enquo(weights)
  weights <- rlang::eval_tidy(weights, .E())
  if (is.null(weights)) {
    weights <- NA
  }
  if (is_bipartite(graph) & normalized){
    closeness <- closeness(graph = graph, v = V(graph), mode = mode, weights = weights, cutoff = cutoff)
    other_set_size <- ifelse(V(graph)$type, sum(!V(graph)$type), sum(V(graph)$type))
    set_size <- ifelse(!V(graph)$type, sum(V(graph)$type), sum(!V(graph)$type))
    closeness/(other_set_size+(2*set_size)-2)
    } else {
      if (is.null(cutoff)) {
        closeness(graph = graph, v = V(graph), mode = mode, weights = weights, normalized = normalized)
      } else {
        estimate_closeness(graph = graph, vids = V(graph), mode = mode, cutoff = cutoff, weights = weights, normalized = normalized)
      }
    }
} 

# #' Betweenness centrality for two-mode networks
#' 
#' This function substitutes tidygraph::centrality_betweenness()
#' with a version that correctly normalizes for two-mode networks.
#

#' Title
#'
#' @param weights 
#' @param directed 
#' @param cutoff 
#' @param nobigint 
#' @param normalized 
#' @family two-mode functions 
#' @references Borgatti, Stephen P., and Martin G. Everett. "Network analysis of 2-mode data." Social networks 19.3 (1997): 243-270.
#' @return
#' @export
#'
#' @examples
#' 

centrality_betweenness <- function(weights = NULL, directed = TRUE, cutoff = NULL, nobigint = TRUE, normalized = FALSE){
  tidygraph:::expect_nodes()
  graph <- .G()
  weights <- enquo(weights)
  weights <- rlang::eval_tidy(weights, .E())
  if (is.null(weights)) {
    weights <- NA
  } 
  if (is_bipartite(graph) & normalized){
    betweenness <- betweenness(graph = graph, v = V(graph), mode = mode, weights = weights, cutoff = cutoff)
    other_set_size <- ifelse(V(graph)$type, sum(!V(graph)$type), sum(V(graph)$type))
    set_size <- ifelse(!V(graph)$type, sum(V(graph)$type), sum(!V(graph)$type))
    ifelse (set_size > other_set_size, betweenness/(2*(set_size-1)(other_set_size-1)), betweenness/(1/2*other_set_size(other_set_size-1)+1/2(set_size-1)*(set_size-2)+(set_size-1)*(other_set_size-1)))
   } else {
    if (is.null(cutoff)) {
    betweenness(graph = graph, v = V(graph), directed = directed, weights = weights, nobigint = nobigint, normalized = normalized)
   } else {
    estimate_betweenness(graph = graph, vids = V(graph), directed = directed, cutoff = cutoff, weights = weights, nobigint = nobigint)
  }
}
}
