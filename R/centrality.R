#' Centrality for one- and two-mode networks
#'
#' These functions calculate common centrality measures for both one- and two-mode networks.
#' They accept as objects matrices and `igraph` graphs, 
#' and can be used within a tidygraph workflow.
#' Importantly, these functions also offer correct normalization for two-mode networks.
#' @name centrality
#' @family two-mode measures
#' @family node-level measures
#' @inheritParams is
#' @param weights The weight of the edges to use for the calculation. 
#' Will be evaluated in the context of the edge data.
#' @param mode How should edges be followed (in or out). By default, outdegree of
#' the node is calculated. Ignored for undirected graphs.
#' @param loops Should loops be included in the calculation
#' @param normalized Should the score be normalized. By default TRUE.
#' @importFrom rlang enquo eval_tidy
#' @importFrom igraph graph_from_incidence_matrix is_bipartite degree V
#' @references 
#' Borgatti, Stephen P., and Martin G. Everett (1997). "Network analysis of 2-mode data." _Social Networks_ 19(3): 243-270.
#' 
#' Faust, Katherine (1997). "Centrality in affiliation networks." _Social Networks_ 19(2): 157-191.
#' @examples
#' node_degree(mpn_elite_mex)
#' node_degree(ison_southern_women)
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
    degrees <- igraph::degree(graph = graph, 
                              v = igraph::V(graph), 
                              mode = mode, loops = loops)
    other_set_size <- ifelse(igraph::V(graph)$type, 
                             sum(!igraph::V(graph)$type), 
                             sum(igraph::V(graph)$type))
    out <- degrees/other_set_size
  } else {
    if (is.null(weights)) {
      out <- igraph::degree(graph = graph, V = igraph::V(graph), 
                     mode = mode, loops = loops,
                     normalized = normalized)
    }
    else {
      out <- igraph::strength(graph = graph, vids = igraph::V(graph), 
                       mode = mode,
                       loops = loops, weights = weights)
    }
  }
  out <- make_measure(out, object)
  out
}

#' @describeIn centrality Calculate the degree centralization for a graph
#' @examples
#' graph_degree(ison_southern_women, directed = "in")
#' @export
graph_degree <- function(object,
                         directed = c("all", "out", "in", "total"), 
                         normalized = TRUE, 
                         digits = 2){
  
  directed <- match.arg(directed)
  
  if (is_twomode(object)) {
    mat <- as_matrix(object)
    mode <- c(rep(FALSE, nrow(mat)), rep(TRUE, ncol(mat)))
    
    out <- list()
    if (directed == "all") {
      if (!normalized) {
        allcent <- c(rowSums(mat), colSums(mat))
        out$nodes1 <- sum(max(allcent[!mode]) - allcent)/((nrow(mat) + ncol(mat))*ncol(mat) - 2*(ncol(mat) + nrow(mat) - 1))
        out$nodes2 <- sum(max(allcent[mode]) - allcent)/((nrow(mat) + ncol(mat))*nrow(mat) - 2*(ncol(mat) + nrow(mat) - 1))
      } else if (normalized) {
        allcent <- node_degree(mat, normalized = TRUE)
        out$nodes1 <- sum(max(allcent[!mode]) - allcent)/((nrow(mat) + ncol(mat) - 1) - (ncol(mat) - 1) / nrow(mat) - (ncol(mat) + nrow(mat) - 1)/nrow(mat))
        out$nodes2 <- sum(max(allcent[mode]) - allcent)/((ncol(mat) + nrow(mat) - 1) - (nrow(mat) - 1) / ncol(mat) - (nrow(mat)  + ncol(mat) - 1)/ncol(mat))
      }
    } else if (directed == "in") {
      out$nodes1 <- sum(max(rowSums(mat)) - rowSums(mat))/((ncol(mat) - 1)*(nrow(mat) - 1))
      out$nodes2 <- sum(max(colSums(mat)) - colSums(mat))/((ncol(mat) - 1)*(nrow(mat) - 1))
    }
    if(!isFALSE(digits)) out <- lapply(out, round, digits)
  } else {
    out <- igraph::centr_degree(graph = object, mode = directed, normalized = normalized)$centralization
    if(!isFALSE(digits)) out <- round(out, digits)
  }
  out
}

#' @describeIn centrality Calculate the closeness centrality of nodes in a network
#' @param cutoff maximum path length to use during calculations 
#' @import tidygraph
#' @examples
#' node_closeness(mpn_elite_mex)
#' node_closeness(ison_southern_women)
#' @export
node_closeness <- function (object, 
                            weights = NULL, mode = "out", 
                            normalized = TRUE, cutoff = NULL){
  
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
    out <- closeness/(1/(other_set_size+2*set_size-2))
    } else {
      if (is.null(cutoff)) {
        out <- igraph::closeness(graph = graph, vids = igraph::V(graph), mode = mode,
                  weights = weights, normalized = normalized)
      } else {
        out <- igraph::estimate_closeness(graph = graph, vids = igraph::V(graph), mode = mode, 
                           cutoff = cutoff, weights = weights, normalized = normalized)
      }
    }
  out <- make_measure(out, object)
  out
} 

#' @describeIn centrality Calculate the closeness of each edge to each other edge
#' in the network.
#' @examples
#' (ec <- edge_closeness(ison_adolescents))
#' plot(ec)
#' ison_adolescents %>% 
#'   activate(edges) %>% mutate(weight = ec) %>% 
#'   autographr()
#' @export
edge_closeness <- function(object){
  edge_adj <- to_edges(object)
  node_closeness(edge_adj)
}

#' @describeIn centrality Calculate the closeness centralization for a graph
#' @examples
#' graph_closeness(ison_southern_women, directed = "in")
#' @export
graph_closeness <- function(object,
                            directed = c("all", "out", "in", "total"), 
                            normalized = TRUE, 
                            digits = 2){
  
  directed <- match.arg(directed)
  graph <- as_igraph(object)
  
  if (is_twomode(object)) {
    clcent <- node_closeness(graph, normalized = TRUE)
    mode <- igraph::V(graph)$type
    mode1 <- length(mode) - sum(mode)
    mode2 <- sum(mode)
    out <- list()
    if (directed == "in") {
      out$nodes1 <- sum(max(clcent[!mode]) - clcent[!mode])/(((mode1 - 2)*(mode1 - 1))/(2 * mode1 - 3))
      out$nodes2 <- sum(max(clcent[mode]) - clcent[mode])/(((mode2 - 2)*(mode2 - 1))/(2 * mode2 - 3))
      if (mode1 > mode2) { #28.43
        lhs <- ((mode2 - 1)*(mode1 - 2) / (2 * mode1 - 3))
        rhs <- ((mode2 - 1)*(mode1 - mode2) / (mode1 + mode2 - 2))
        out$nodes1 <- sum(max(clcent[!mode]) - clcent[!mode])/( lhs +  rhs) # 0.2135
      }
      if (mode2 > mode1) {
        lhs <- ((mode1 - 1)*(mode2 - 2) / (2 * mode2 - 3))
        rhs <- ((mode1 - 1)*(mode2 - mode1) / (mode2 + mode1 - 2))
        out$nodes2 <- sum(max(clcent[mode]) - clcent[mode])/( lhs +  rhs)
      }
    } else {
      term1 <- 2*(mode1 - 1) * (mode2 + mode1 - 4)/(3*mode2 + 4*mode1 - 8)
      term2 <- 2*(mode1 - 1) * (mode1 - 2)/(2*mode2 + 3*mode1 - 6)
      term3 <- 2*(mode1 - 1) * (mode2 - mode1 + 1)/(2*mode2 + 3*mode1 - 4)
      out$nodes1 <- sum(max(clcent[!mode]) - clcent) / sum(term1, term2, term3)
      term1 <- 2*(mode2 - 1) * (mode1 + mode2 - 4)/(3*mode1 + 4*mode2 - 8)
      term2 <- 2*(mode2 - 1) * (mode2 - 2)/(2*mode1 + 3*mode2 - 6)
      term3 <- 2*(mode2 - 1) * (mode1 - mode2 + 1)/(2*mode1 + 3*mode2 - 4)
      out$nodes2 <- sum(max(clcent[mode]) - clcent) / sum(term1, term2, term3)
      
      if (mode1 > mode2) {
        term1 <- 2*(mode2 - 1) * (mode2 + mode1 - 2) / (3 * mode2 + 4 * mode1 - 8)
        term2 <- 2*(mode1 - mode2) * (2 * mode2 - 1) / (5 * mode2 + 2 * mode1 - 6)
        term3 <- 2*(mode2 - 1) * (mode1 - 2) / (2 * mode2 + 3 * mode1 - 6)
        term4 <- 2 * (mode2 - 1) / (mode1 + 4 * mode2 - 4)
        out$nodes1 <- sum(max(clcent[!mode]) - clcent) / sum(term1, term2, term3, term4)
      }
      if (mode2 > mode1) {
        term1 <- 2*(mode1 - 1) * (mode1 + mode2 - 2) / (3 * mode1 + 4 * mode2 - 8)
        term2 <- 2*(mode2 - mode1) * (2 * mode1 - 1) / (5 * mode1 + 2 * mode2 - 6)
        term3 <- 2*(mode1 - 1) * (mode2 - 2) / (2 * mode1 + 3 * mode2 - 6)
        term4 <- 2 * (mode1 - 1) / (mode2 + 4 * mode1 - 4)
        out$nodes2 <- sum(max(clcent[mode]) - clcent) / sum(term1, term2, term3, term4)
      }
    }
    if(!isFALSE(digits)) out <- lapply(out, round, digits)
  } else {
    out <- igraph::centr_clo(graph = graph,
                             mode = directed,
                             normalized = normalized)$centralization
    if(!isFALSE(digits)) out <- round(out, digits)
  }
  out
}

#' @describeIn centrality Calculate the betweenness centralities of nodes in a network
#' @param directed Should direction of edges be used for the calculations 
#' @param cutoff maximum path length to use during calculations
#' @param nobigint Should big integers be avoided during calculations 
#' @import tidygraph
#' @examples
#' node_betweenness(mpn_elite_mex)
#' node_betweenness(ison_southern_women)
#' @return A numeric vector giving the betweenness centrality measure of each node.
#' @export 
node_betweenness <- function(object, 
                                   weights = NULL, directed = TRUE,
                                   cutoff = NULL, nobigint = TRUE, normalized = TRUE){

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
    out <- ifelse(set_size > other_set_size, 
           betw_scores/(2*(set_size-1)*(other_set_size-1)), 
           betw_scores/(1/2*other_set_size*(other_set_size-1)+1/2*(set_size-1)*(set_size-2)+(set_size-1)*(other_set_size-1)))
  } else {
    if (is.null(cutoff)) {
      out <- igraph::betweenness(graph = graph, v = igraph::V(graph), directed = directed, weights = weights, nobigint = nobigint, normalized = normalized)
    } else {
      out <- igraph::estimate_betweenness(graph = graph, vids = igraph::V(graph), directed = directed, cutoff = cutoff, weights = weights, nobigint = nobigint)
    }
  }
  out <- make_measure(out, object)
  out
}

#' @describeIn centrality Calculate number of shortest paths going through an edge
#' @importFrom igraph edge_betweenness
#' @examples
#' (eb <- edge_betweenness(ison_adolescents))
#' plot(eb)
#' ison_adolescents %>% 
#'   activate(edges) %>% mutate(weight = eb) %>% 
#'   autographr()
#' @export
edge_betweenness <- function(object){
  object <- as_igraph(object)
  edges <- as_edgelist(object)
  edges <- paste(edges$from, edges$to, sep = "-")
  out <- igraph::edge_betweenness(object)
  names(out) <- edges
  class(out) <- c("measure", class(out))
  out
}

#' @describeIn centrality Calculate the betweenness centralization for a graph
#' @examples
#' graph_betweenness(ison_southern_women, directed = "in")
#' @export
graph_betweenness <- function(object,
                              directed = c("all", "out", "in", "total"), 
                              normalized = TRUE, 
                              digits = 2) {
  
  directed <- match.arg(directed)
  graph <- as_igraph(object)
  
  if (is_twomode(object)) {
    becent <- node_betweenness(graph, normalized = FALSE)
    mode <- igraph::V(graph)$type
    mode1 <- length(mode) - sum(mode)
    mode2 <- sum(mode)
    out <- list()
    if (directed == "all") {
      if (!normalized) {
        out$nodes1 <- sum(max(becent[!mode]) - becent) / ((1/2 * mode2 * (mode2 - 1) + 1/2 * (mode1 - 1)*(mode1 - 2) + (mode1 - 1) * (mode2 - 2))*(mode1 + mode2 - 1) + (mode1 - 1))
        out$nodes2 <- sum(max(becent[mode]) - becent) / ((1/2 * mode1 * (mode1 - 1) + 1/2 * (mode2 - 1)*(mode2 - 2) + (mode2 - 1) * (mode1 - 2))*(mode2 + mode1 - 1) + (mode2 - 1))
        if (mode1 > mode2) {
          out$nodes1 <- sum(max(becent[!mode]) - becent) / (2 * (mode1 - 1) * (mode2 - 1) * (mode1 + mode2 - 1) - (mode2 - 1) * (mode1 + mode2 - 2) - 1/2 * (mode1 - mode2) * (mode1 + 3*mode2 - 3))
        }
        if (mode2 > mode1) {
          out$nodes2 <- sum(max(becent[mode]) - becent) / (2 * (mode2 - 1) * (mode1 - 1) * (mode2 + mode1 - 1) - (mode1 - 1) * (mode2 + mode1 - 2) - 1/2 * (mode2 - mode1) * (mode2 + 3*mode1 - 3))
        }
      } else if (normalized) {
        out$nodes1 <- sum(max(becent[!mode]) - becent) / ((1/2 * mode2 * (mode2 - 1) + 1/2 * (mode1 - 1)*(mode1 - 2) + (mode1 - 1) * (mode2 - 2))*(mode1 + mode2 - 1) + (mode1 - 1))
        out$nodes2 <- sum(max(becent[mode]) - becent) / ((1/2 * mode1 * (mode1 - 1) + 1/2 * (mode2 - 1)*(mode2 - 2) + (mode2 - 1) * (mode1 - 2))*(mode2 + mode1 - 1) + (mode2 - 1))
        if (mode1 > mode2) {
          becent <- node_betweenness(graph, normalized = TRUE)
          out$nodes1 <- sum(max(becent[!mode]) - becent) / ((mode1 + mode2 - 1) - (((mode2 - 1)*(mode1 + mode2 - 2) + 1/2*(mode1 - mode2)*(mode1 + (3*mode2) - 3)) / (1/2*(mode1*(mode1 - 1)) + 1/2*(mode2 - 1) * (mode2 - 2) + (mode1 - 1) * (mode2 - 1))))
        }
        if (mode2 > mode1) {
          becent <- node_betweenness(graph, normalized = TRUE)
          out$nodes2 <- sum(max(becent[mode]) - becent) / ((mode1 + mode2 - 1)*((mode1 - 1)*(mode1 + mode2 - 2) / 2*(mode1 - 1)*(mode2 - 1)))
        }
      }
    } else if (directed == "in") {
      out$nodes1 <- sum(max(becent[!mode]) - becent[!mode])/((mode1 - 1)*(1/2*mode2*(mode2 - 1) + 1/2*(mode1 - 1)*(mode1 - 2) + (mode1 - 1)*(mode2 - 1)))
      out$nodes2 <- sum(max(becent[mode]) - becent[mode])/((mode2 - 1)*(1/2*mode1*(mode1 - 1) + 1/2 * (mode2 - 1) * (mode2 - 2) + (mode2 - 1) * (mode1 - 1)))
      if (mode1 > mode2) {
        out$nodes1 <- sum(max(becent[!mode]) - becent[!mode]) / (2 * (mode1 - 1)^2 * (mode2 - 1))
      }
      if (mode2 > mode1) {
        out$nodes2 <- sum(max(becent[mode]) - becent[mode]) / (2 * (mode2 - 1)^2 * (mode1 - 1))
      }
    }
    if(!isFALSE(digits)) out <- lapply(out, round, digits)
  } else {
    out <- igraph::centr_betw(graph = graph)$centralization
    if(!isFALSE(digits)) out <- round(out, digits)
  }
  out
}

#' @describeIn centrality Calculate the eigenvector centrality of nodes in a network
#' @param scale Should the scores be scaled to range between 0 and 1? 
#' @param normalized For one-mode networks, should Borgatti and Everett normalization be applied?
#' @examples
#' node_eigenvector(mpn_elite_mex)
#' node_eigenvector(ison_southern_women)
#' @return A numeric vector giving the eigenvector centrality measure of each node.
#' @export 
node_eigenvector <- function(object, 
                             weights = NULL, directed = FALSE,
                             scale = FALSE, normalized = TRUE){
  
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
    out <- igraph::eigen_centrality(graph = graph, 
                                    directed = directed, scale = scale, 
                                    options = igraph::arpack_defaults)$vector
    if (normalized) out <- out / sqrt(1/2)
  } else {
    eigen1 <- to_mode1(graph)
    eigen1 <- igraph::eigen_centrality(graph = eigen1, 
                                       directed = directed, scale = scale, 
                                       options = igraph::arpack_defaults)$vector
    eigen2 <- to_mode2(graph)
    eigen2 <- igraph::eigen_centrality(graph = eigen2, 
                                       directed = directed, scale = scale, 
                                       options = igraph::arpack_defaults)$vector
    out <- c(eigen1, eigen2)
    if (normalized) stop("Normalization not currently implemented for eigenvector centrality for two-mode networks.")
  }
  out <- make_measure(out, object)
  out
}

#' @describeIn centrality Calculate the eigenvector centralization for a graph
#' @examples
#' graph_eigenvector(mpn_elite_mex)
#' @export
graph_eigenvector <- function(object, digits = 2){
  if (is_twomode(object)) {
    stop("Eignevector centrality for two-mode networks is not yet implemented.")
  } else {
    out <- igraph::centr_eigen(as_igraph(object))$centralization
    if(!isFALSE(digits)) out <- round(out, digits)
  }
  out
}
