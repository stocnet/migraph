#' Centrality for one- and two-mode networks
#'
#' These functions calculate common centrality measures for both one- and two-mode networks.
#' They accept as objects matrices and `igraph` graphs, 
#' and can be used within a tidygraph workflow.
#' Importantly, these functions also offer correct normalization for two-mode networks.
#' @name centrality
#' @family two-mode measures
#' @family node-level measures
#' @param object Either an igraph graph object or a matrix.
#' @param weights The weight of the edges to use for the calculation. 
#' Will be evaluated in the context of the edge data.
#' @param mode How should edges be followed. Ignored for undirected graphs
#' @param loops Should loops be included in the calculation
#' @param normalized Should the output be normalized for one or two-modes networks
#' @importFrom rlang enquo eval_tidy
#' @importFrom igraph graph_from_incidence_matrix is_bipartite degree V
#' @references 
#' Borgatti, Stephen P., and Martin G. Everett. "Network analysis of 2-mode data." Social networks 19.3 (1997): 243-270.
#' 
#' Faust, Katherine. "Centrality in affiliation networks." Social networks 19.2 (1997): 157-191.
#' @examples
#' node_degree(mpn_elite_mex)
#' node_degree(southern_women)
#' @return Depending on how and what kind of an object is passed to the function,
#' the function will return a `tidygraph` object where the nodes have been updated
#' @export
node_degree <- function (object, 
                         weights = NULL, mode = "out", 
                         loops = TRUE, normalized = FALSE){
  
  if(missing(object)){
    expect_nodes()
    object <- .G()
  }
  graph <- as_igraph(object)
  
  # Do the calculations
  if (is.null(weights)) {
    weights <- NA
  }
  if (is_twomode(graph) & normalized){
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

#' @rdname centrality
#' @param cutoff maximum path length to use during calculations 
#' @import tidygraph
#' @examples
#' node_closeness(mpn_elite_mex)
#' node_closeness(southern_women)
#' @export
node_closeness <- function (object, 
                            weights = NULL, mode = "out", 
                            normalized = FALSE, cutoff = NULL){
  
  if(missing(object)){
    expect_nodes()
    object <- .G()
  }
  graph <- as_igraph(object)
  
  # Do the calculations
  if (is.null(weights)) {
    weights <- NA
  }
  if (is_twomode(graph) & normalized){
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

#' @rdname centrality
#' @param directed Should direction of edges be used for the calculations 
#' @param cutoff maximum path length to use during calculations
#' @param nobigint Should big integers be avoided during calculations 
#' @import tidygraph
#' @examples
#' node_betweenness(mpn_elite_mex)
#' node_betweenness(southern_women)
#' @return A numeric vector giving the betweenness centrality measure of each node.
#' @export 
node_betweenness <- function(object, 
                                   weights = NULL, directed = TRUE,
                                   cutoff = NULL, nobigint = TRUE, normalized = FALSE){

  if(missing(object)){
    expect_nodes()
    object <- .G()
  }
  graph <- as_igraph(object)
  
  # Do the calculations
  if (is.null(weights)) {
    weights <- NA
  } 
  if (igraph::is_bipartite(graph) & normalized){
    betw_scores <- igraph::betweenness(graph = graph, v = igraph::V(graph), directed = directed, nobigint = nobigint)
    other_set_size <- ifelse(igraph::V(graph)$type, sum(!igraph::V(graph)$type), sum(igraph::V(graph)$type))
    set_size <- ifelse(igraph::V(graph)$type, sum(igraph::V(graph)$type), sum(!igraph::V(graph)$type))
    ifelse(set_size > other_set_size, 
           betw_scores/(2*(set_size-1)*(other_set_size-1)), 
           betw_scores/(1/2*other_set_size*(other_set_size-1)+1/2*(set_size-1)*(set_size-2)+(set_size-1)*(other_set_size-1)))
  } else {
    if (is.null(cutoff)) {
      igraph::betweenness(graph = graph, v = igraph::V(graph), directed = directed, weights = weights, nobigint = nobigint, normalized = normalized)
    } else {
      igraph::estimate_betweenness(graph = graph, vids = igraph::V(graph), directed = directed, cutoff = cutoff, weights = weights, nobigint = nobigint)
    }
  }
}

#' @rdname centrality
#' @param options Settings passed on to `igraph::arpack()`
#' @param scale Should the scores be scaled to range between 0 and 1? 
#' @param normalized For one-mode networks, should Borgatti and Everett normalization be applied?
#' @examples
#' node_eigenvector(mpn_elite_mex)
#' node_eigenvector(southern_women)
#' @return A numeric vector giving the eigenvector centrality measure of each node.
#' @export 
node_eigenvector <- function(object, 
                                   weights = NULL, directed = FALSE,
                                   options = igraph::arpack_defaults, 
                                   scale = FALSE, normalized = FALSE){
  
  if(missing(object)){
    expect_nodes()
    object <- .G()
  }
  graph <- as_igraph(object)
  
  # Do the calculations
  if (is.null(weights)) {
    weights <- NA
  }
  if (!is_twomode(graph)){
    out <- igraph::eigen_centrality(graph = graph, directed = directed, scale = scale, options = options)$vector
    if (normalized) out <- out / sqrt(1/2)
  } else {
    eigen1 <- project_rows(graph)
    eigen1 <- igraph::eigen_centrality(graph = eigen1, directed = directed, scale = scale, options = options)$vector
    eigen2 <- project_cols(graph)
    eigen2 <- igraph::eigen_centrality(graph = eigen2, directed = directed, scale = scale, options = options)$vector
    out <- c(eigen1, eigen2)
    if (normalized) stop("Normalization not currently implemented for eigenvector centrality for two-mode networks.")
  }
  out
}

