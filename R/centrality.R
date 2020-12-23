#' Degree centrality for two-mode networks
#'
#' This function substitutes tidygraph::centrality_degree()
#' with a version that correctly normalizes two-mode networks.
#'
#' @param weights The weight of the edges to use for the calculation. 
#' Will be evaluated in the context of the edge data.
#' @param mode How should edges be followed. Ignored for undirected graphs
#' @param loops Should loops be included in the calculation
#' @param normalized Should the output be normalized for one or two-modes networks
#' @family two-mode functions
#' @references Borgatti, Stephen P., and Martin G. Everett. "Network analysis of 2-mode data." Social networks 19.3 (1997): 243-270.
#' @examples
#' data(southern_women)
#' southern_women <- as_tbl_graph(southern_women)
#' with_graph(southern_women, migraph::centrality_degree(normalized = TRUE))
#' @return A numeric vector giving the degree centrality measure of each node.
#' @export
centrality_degree <- function (weights = NULL, mode = "out", loops = TRUE, normalized = FALSE){
  tidygraph:::expect_nodes()
  graph <- .G()
  weights <- enquo(weights)
  weights <- rlang::eval_tidy(weights, .E())
  if (is.null(weights)) {
    weights <- NA
  }
  if (is_bipartite(graph) & normalized){
    degrees <- igraph::degree(graph = graph, v = igraph::V(graph), mode = mode, loops = loops)
    other_set_size <- ifelse(igraph::V(graph)$type, sum(!igraph::V(graph)$type), sum(igraph::V(graph)$type))
    degrees/other_set_size
  } else {
    if (is.null(weights)) {
      igraph::degree(graph = graph, V = igraph::V(graph), mode = mode, loops = loops, 
             normalized = normalized)
    }
    else {
      igraph::strength(graph = graph, vids = igraph::V(graph), mode = mode, 
               loops = loops, weights = weights)
    }
  }
}

#' Closeness centrality for two-mode networks
#' 
#' This function substitutes tidygraph::centrality_closeness()
#' with a version that correctly normalizes two-mode networks.
#'
#' @param The weight of the edges to use for the calculation. Will be
#' evaluated in the context of the edge data.
#' @param mode How should edges be followed. Ignored for undirected graphs
#' @param cutoff maximum path length to use during calculations 
#' @param normalized Should the output be normalized for one or two-mode networks
#' @family two-mode functions
#' @references Borgatti, Stephen P., and Martin G. Everett. "Network analysis of 2-mode data." Social networks 19.3 (1997): 243-270.
#' @examples
#' data(southern_women)
#' southern_women <- as_tbl_graph(southern_women)
#' with_graph(southern_women, migraph::centrality_closeness(normalized = TRUE))
#' @return A numeric vector giving the closeness centrality measure of each node.
#' @export
centrality_closeness <- function (weights = NULL, mode = "out", normalized = FALSE, cutoff = NULL){
  tidygraph:::expect_nodes()
  graph <- .G()
  weights <- enquo(weights)
  weights <- rlang::eval_tidy(weights, .E())
  if (is.null(weights)) {
    weights <- NA
  }
  if (igraph::is_bipartite(graph) & normalized){
    # farness <- rowSums(igraph::distances(graph = graph))
    closeness <- igraph::closeness(graph = graph, vids = igraph::V(graph), mode = mode)
    other_set_size <- ifelse(igraph::V(graph)$type, sum(!igraph::V(graph)$type), sum(igraph::V(graph)$type))
    set_size <- ifelse(igraph::V(graph)$type, sum(igraph::V(graph)$type), sum(!igraph::V(graph)$type))
    closeness/(1/(other_set_size+2*set_size-2))
    } else {
      if (is.null(cutoff)) {
        igraph::closeness(graph = graph, vids = igraph::V(graph), mode = mode,
                  weights = weights, normalized = normalized)
      } else {
        igraph::estimate_closeness(graph = graph, vids = igraph::V(graph), mode = mode, 
                           cutoff = cutoff, weights = weights, normalized = normalized)
      }
    }
} 

# #' Betweenness centrality for two-mode networks
#' 
#' This function substitutes tidygraph::centrality_betweenness()
#' with a version that correctly normalizes for two-mode networks.
#' 
#' @param The weight of the edges to use for the calculation. Will be
#' evaluated in the context of the edge data. 
#' @param directed Should direction of edges be used for the calculations 
#' @param cutoff maximum path length to use during calculations
#' @param nobigint Should big integers be avoided during calculations 
#' @param normalized Should the output be normalized for one or two-mode networks 
#' @family two-mode functions 
#' @references Borgatti, Stephen P., and Martin G. Everett. "Network analysis of 2-mode data." Social networks 19.3 (1997): 243-270.
#' @examples
#' data(southern_women)
#' southern_women <- as_tbl_graph(southern_women)
#' with_graph(southern_women, migraph::centrality_betweenness(normalized = TRUE))
#' @return A numeric vector giving the betweenness centrality measure of each node.
#' @export 
centrality_betweenness <- function(weights = NULL, directed = TRUE, cutoff = NULL, nobigint = TRUE, normalized = FALSE){
  tidygraph:::expect_nodes()
  graph <- .G()
  weights <- enquo(weights)
  weights <- rlang::eval_tidy(weights, .E())
  if (is.null(weights)) {
    weights <- NA
  } 
  if (is_bipartite(graph) & normalized){
    betweenness <- betweenness(graph = graph, v = igraph::V(graph), directed = directed, nobigint = nobigint)
    other_set_size <- ifelse(igraph::V(graph)$type, sum(!igraph::V(graph)$type), sum(igraph::V(graph)$type))
    set_size <- ifelse(igraph::V(graph)$type, sum(igraph::V(graph)$type), sum(!igraph::V(graph)$type))
    ifelse(set_size > other_set_size, 
            betweenness/(2*(set_size-1)*(other_set_size-1)), 
            betweenness/(1/2*other_set_size*(other_set_size-1)+1/2*(set_size-1)*(set_size-2)+(set_size-1)*(other_set_size-1)))
   } else {
    if (is.null(cutoff)) {
    betweenness(graph = graph, v = V(graph), directed = directed, weights = weights, nobigint = nobigint, normalized = normalized)
   } else {
    estimate_betweenness(graph = graph, vids = V(graph), directed = directed, cutoff = cutoff, weights = weights, nobigint = nobigint)
  }
}
}
