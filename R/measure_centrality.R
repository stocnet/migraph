#' Measures of node and tie centrality
#'
#' @description
#'   These functions calculate common centrality measures for one- and two-mode networks.
#'   All measures attempt to use as much information as they are offered,
#'   including whether the networks are directed, weighted, or multimodal.
#'   If this would produce unintended results, 
#'   first transform the salient properties using e.g. [to_undirected()] functions.
#'   All centrality and centralization measures return normalized measures by default,
#'   including for two-mode networks.
#' @name centrality
#' @family measures
#' @seealso [to_undirected()] for removing edge directions
#'   and [to_unweighted()] for removing weights from a graph.
#' @inheritParams is
#' @param normalized Logical scalar, whether the centrality scores are normalized.
#'   Different denominators are used depending on whether the object is one-mode or two-mode,
#'   the type of centrality, and other arguments.
#' @param direction Character string, “out” bases the measure on outgoing ties, 
#'   “in” on incoming ties, and "all" on either/the sum of the two. 
#'   For two-mode networks, "all" uses as numerator the sum of differences
#'   between the maximum centrality score for the mode 
#'   against all other centrality scores in the network,
#'   whereas "in" uses as numerator the sum of differences
#'   between the maximum centrality score for the mode 
#'   against only the centrality scores of the other nodes in that mode.
#' @return A single centralization score if the object was one-mode,
#'   and two centralization scores if the object was two-mode.
#' @importFrom rlang enquo eval_tidy
#' @importFrom igraph graph_from_incidence_matrix is_bipartite degree V
#' @references 
#' Faust, Katherine. 1997. 
#' "Centrality in affiliation networks." 
#' _Social Networks_ 19(2): 157-191.
#' \doi{10.1016/S0378-8733(96)00300-0}.
#' 
#' Borgatti, Stephen P., and Martin G. Everett. 1997. 
#' "Network analysis of 2-mode data." 
#' _Social Networks_ 19(3): 243-270.
#' \doi{10.1016/S0378-8733(96)00301-2}.
#' 
#' Borgatti, Stephen P., and Daniel S. Halgin. 2011. 
#' "Analyzing affiliation networks." 
#' In _The SAGE Handbook of Social Network Analysis_, 
#' edited by John Scott and Peter J. Carrington, 417–33. 
#' London, UK: Sage.
#' \doi{10.4135/9781446294413.n28}.
#' @examples
#' node_degree(mpn_elite_mex)
#' node_degree(ison_southern_women)
#' @return Depending on how and what kind of an object is passed to the function,
#' the function will return a `tidygraph` object where the nodes have been updated
NULL

#' @describeIn centrality Calculates the degree centrality of nodes in an unweighted network,
#'   or weighted degree/strength of nodes in a weighted network.
#' @export
node_degree <- function (object, normalized = TRUE, 
                         direction = c("all","out","in")){
  
  if(missing(object)){
    expect_nodes()
    object <- .G()
  }
  graph <- as_igraph(object)
  weights <- `if`(is_weighted(object), 
                    tie_weights(object), NA)
  direction <- match.arg(direction)
  
  # Do the calculations
  if (is_twomode(graph) & normalized){
    degrees <- igraph::degree(graph = graph, 
                              v = igraph::V(graph), 
                              mode = direction, 
                              loops = is_complex(object))
    other_set_size <- ifelse(igraph::V(graph)$type, 
                             sum(!igraph::V(graph)$type), 
                             sum(igraph::V(graph)$type))
    out <- degrees/other_set_size
  } else {
    if (all(is.na(weights))) {
      out <- igraph::degree(graph = graph, v = igraph::V(graph), 
                     mode = direction, 
                     loops = is_complex(object),
                     normalized = normalized)
    }
    else {
      out <- igraph::strength(graph = graph, vids = igraph::V(graph), 
                       mode = direction,
                       loops = is_complex(object), weights = weights)
    }
  }
  out <- make_node_measure(out, object)
  out
}

#' @describeIn centrality Calculate the degree centrality of edges in a network
#' @examples 
#' tie_degree(ison_adolescents)
#' @export
tie_degree <- function(object, normalized = TRUE){
  edge_adj <- to_ties(object)
  out <- node_degree(edge_adj, normalized = normalized)
  class(out) <- "numeric"
  out <- make_tie_measure(out, object)
  out
}

#' @describeIn centrality Calculate the closeness centrality of nodes in a network
#' @param cutoff Maximum path length to use during calculations.
#' @import tidygraph
#' @examples
#' node_closeness(mpn_elite_mex)
#' node_closeness(ison_southern_women)
#' @export
node_closeness <- function(object, normalized = TRUE, 
                           direction = "out", cutoff = NULL){
  
  if(missing(object)){
    expect_nodes()
    object <- .G()
  }
  weights <- `if`(is_weighted(object), 
                  tie_weights(object), NA)
  graph <- as_igraph(object)
  
  # Do the calculations
  if (is_twomode(graph) & normalized){
    # farness <- rowSums(igraph::distances(graph = graph))
    closeness <- igraph::closeness(graph = graph, vids = igraph::V(graph), mode = direction)
    other_set_size <- ifelse(igraph::V(graph)$type, sum(!igraph::V(graph)$type), sum(igraph::V(graph)$type))
    set_size <- ifelse(igraph::V(graph)$type, sum(igraph::V(graph)$type), sum(!igraph::V(graph)$type))
    out <- closeness/(1/(other_set_size+2*set_size-2))
    } else {
      if (is.null(cutoff)) {
        out <- igraph::closeness(graph = graph, vids = igraph::V(graph), mode = direction,
                  weights = weights, normalized = normalized)
      } else {
        out <- igraph::estimate_closeness(graph = graph, vids = igraph::V(graph), mode = direction, 
                           cutoff = cutoff, weights = weights, normalized = normalized)
      }
    }
  out <- make_node_measure(out, object)
  out
} 

#' @describeIn centrality Calculate the closeness of each edge to each other edge
#' in the network.
#' @examples
#' (ec <- tie_closeness(ison_adolescents))
#' plot(ec)
#' ison_adolescents %>% 
#'   activate(edges) %>% mutate(weight = ec) %>% 
#'   autographr()
#' @export
tie_closeness <- function(object, normalized = TRUE){
  edge_adj <- to_ties(object)
  out <- node_closeness(edge_adj, normalized = normalized)
  class(out) <- "numeric"
  out <- make_tie_measure(out, object)
  out
}

#' @describeIn centrality Calculate the betweenness centralities of nodes in a network
#' @import tidygraph
#' @examples
#' node_betweenness(mpn_elite_mex)
#' node_betweenness(ison_southern_women)
#' @return A numeric vector giving the betweenness centrality measure of each node.
#' @export 
node_betweenness <- function(object, normalized = TRUE, 
                             cutoff = NULL){
  
  if(missing(object)){
    expect_nodes()
    object <- .G()
  }
  weights <- `if`(is_weighted(object), 
                  tie_weights(object), NA)
  graph <- as_igraph(object)
  
  # Do the calculations
  if (is_twomode(graph) & normalized){
    betw_scores <- igraph::betweenness(graph = graph, v = igraph::V(graph), 
                                       directed = is_directed(graph))
    other_set_size <- ifelse(igraph::V(graph)$type, sum(!igraph::V(graph)$type), sum(igraph::V(graph)$type))
    set_size <- ifelse(igraph::V(graph)$type, sum(igraph::V(graph)$type), sum(!igraph::V(graph)$type))
    out <- ifelse(set_size > other_set_size, 
           betw_scores/(2*(set_size-1)*(other_set_size-1)), 
           betw_scores/(1/2*other_set_size*(other_set_size-1)+1/2*(set_size-1)*(set_size-2)+(set_size-1)*(other_set_size-1)))
  } else {
    if (is.null(cutoff)) {
      out <- igraph::betweenness(graph = graph, v = igraph::V(graph), 
                                 directed = is_directed(graph), weights = weights, 
                                 normalized = normalized)
    } else {
      out <- igraph::estimate_betweenness(graph = graph, vids = igraph::V(graph), 
                                          directed = is_directed(graph), cutoff = cutoff, 
                                          weights = weights)
    }
  }
  out <- make_node_measure(out, object)
  out
}

#' @describeIn centrality Calculate number of shortest paths going through an edge
#' @importFrom igraph edge_betweenness
#' @examples
#' (tb <- tie_betweenness(ison_adolescents))
#' plot(tb)
#' ison_adolescents %>% 
#'   activate(edges) %>% mutate(weight = tb) %>% 
#'   autographr()
#' @export
tie_betweenness <- function(object, normalized = TRUE){
  object <- as_igraph(object)
  edges <- as_edgelist(object)
  edges <- paste(edges$from, edges$to, sep = "-")
  out <- igraph::edge_betweenness(object)
  names(out) <- edges
  out <- make_tie_measure(out, object)
  out
}

#' @describeIn centrality Calculate the eigenvector centrality of nodes in a network
#' @param scale Logical scalar, whether to rescale the vector so the maximum score is 1. 
#' @references 
#' Bonacich, Phillip. 1991. 
#' “Simultaneous Group and Individual Centralities.” 
#' _Social Networks_ 13(2):155–68. 
#' \doi{10.1016/0378-8733(91)90018-O}.
#' @examples
#' node_eigenvector(mpn_elite_mex)
#' node_eigenvector(ison_southern_women)
#' @return A numeric vector giving the eigenvector centrality measure of each node.
#' @export 
node_eigenvector <- function(object, normalized = TRUE, scale = FALSE){
  
  if(missing(object)){
    expect_nodes()
    object <- .G()
  }
  weights <- `if`(is_weighted(object), 
                  tie_weights(object), NA)
  graph <- as_igraph(object)
  
  # Do the calculations
  if (!is_twomode(graph)){
    out <- igraph::eigen_centrality(graph = graph, 
                                    directed = is_directed(graph), scale = scale, 
                                    options = igraph::arpack_defaults)$vector
    if (normalized) out <- out / sqrt(1/2)
  } else {
    eigen1 <- to_mode1(graph)
    eigen1 <- igraph::eigen_centrality(graph = eigen1, 
                                       directed = is_directed(eigen1), scale = scale, 
                                       options = igraph::arpack_defaults)$vector
    eigen2 <- to_mode2(graph)
    eigen2 <- igraph::eigen_centrality(graph = eigen2, 
                                       directed = is_directed(eigen2), scale = scale, 
                                       options = igraph::arpack_defaults)$vector
    out <- c(eigen1, eigen2)
    if (normalized) out <- out / sqrt(1/2)
  }
  out <- make_node_measure(out, object)
  out
}

#' @describeIn centrality Calculate the eigenvector centrality of edges in a network
#' @examples 
#' tie_eigenvector(ison_adolescents)
#' @export
tie_eigenvector <- function(object, normalized = TRUE){
  edge_adj <- to_ties(object)
  out <- node_eigenvector(edge_adj, normalized = normalized)
  class(out) <- "numeric"
  out <- make_tie_measure(out, object)
  out
}

#' @describeIn centrality Calculate nodes' reach centrality
#' @param k Integer of steps out to calculate reach
#' @examples
#' node_reach(ison_adolescents)
#' @export
node_reach <- function(object, normalized = TRUE, k = 2){
  out <- rowSums(node_path_census(object)==k)
  if(normalized) out <- out/(graph_nodes(object)-1)
  out <- make_node_measure(out, object)
  out
}

#' Measures of network centralisation
#' @name centralisation
#' @family measures
#' @inheritParams centrality
NULL

#' @describeIn centralisation Calculate the degree centralization for a graph
#' @examples
#' graph_degree(ison_southern_women, direction = "in")
#' @export
graph_degree <- function(object, normalized = TRUE,
                         direction = c("all", "out", "in")){
  
  direction <- match.arg(direction)
  
  if (is_twomode(object)) {
    mat <- as_matrix(object)
    mode <- c(rep(FALSE, nrow(mat)), rep(TRUE, ncol(mat)))
    
    out <- list()
    if (direction == "all") {
      if (!normalized) {
        allcent <- c(rowSums(mat), colSums(mat))
        out$nodes1 <- sum(max(allcent[!mode]) - allcent)/((nrow(mat) + ncol(mat))*ncol(mat) - 2*(ncol(mat) + nrow(mat) - 1))
        out$nodes2 <- sum(max(allcent[mode]) - allcent)/((nrow(mat) + ncol(mat))*nrow(mat) - 2*(ncol(mat) + nrow(mat) - 1))
      } else if (normalized) {
        allcent <- node_degree(mat, normalized = TRUE)
        out$nodes1 <- sum(max(allcent[!mode]) - allcent)/((nrow(mat) + ncol(mat) - 1) - (ncol(mat) - 1) / nrow(mat) - (ncol(mat) + nrow(mat) - 1)/nrow(mat))
        out$nodes2 <- sum(max(allcent[mode]) - allcent)/((ncol(mat) + nrow(mat) - 1) - (nrow(mat) - 1) / ncol(mat) - (nrow(mat)  + ncol(mat) - 1)/ncol(mat))
      }
    } else if (direction == "in") {
      out$nodes1 <- sum(max(rowSums(mat)) - rowSums(mat))/((ncol(mat) - 1)*(nrow(mat) - 1))
      out$nodes2 <- sum(max(colSums(mat)) - colSums(mat))/((ncol(mat) - 1)*(nrow(mat) - 1))
    }
    out <- c("Mode 1" = out$nodes1, "Mode 2" = out$nodes2)
  } else {
    out <- igraph::centr_degree(graph = object, mode = direction, 
                                normalized = normalized)$centralization
  }
  out <- make_graph_measure(out, object)
  out
}

#' @describeIn centralisation Calculate the closeness centralization for a graph
#' @examples
#' graph_closeness(ison_southern_women, direction = "in")
#' @export
graph_closeness <- function(object, normalized = TRUE,
                            direction = c("all", "out", "in")){
  
  direction <- match.arg(direction)
  graph <- as_igraph(object)
  
  if (is_twomode(object)) {
    clcent <- node_closeness(graph, normalized = TRUE)
    mode <- igraph::V(graph)$type
    mode1 <- length(mode) - sum(mode)
    mode2 <- sum(mode)
    out <- list()
    if (direction == "in") {
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
    out <- c("Mode 1" = out$nodes1, "Mode 2" = out$nodes2)
  } else {
    out <- igraph::centr_clo(graph = graph,
                             mode = direction,
                             normalized = normalized)$centralization
  }
  out <- make_graph_measure(out, object)
  out
}

#' @describeIn centralisation Calculate the betweenness centralization for a graph
#' @examples
#' graph_betweenness(ison_southern_women, direction = "in")
#' @export
graph_betweenness <- function(object, normalized = TRUE,
                              direction = c("all", "out", "in")) {
  
  direction <- match.arg(direction)
  graph <- as_igraph(object)
  
  if (is_twomode(object)) {
    becent <- node_betweenness(graph, normalized = FALSE)
    mode <- igraph::V(graph)$type
    mode1 <- length(mode) - sum(mode)
    mode2 <- sum(mode)
    out <- list()
    if (direction == "all") {
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
    } else if (direction == "in") {
      out$nodes1 <- sum(max(becent[!mode]) - becent[!mode])/((mode1 - 1)*(1/2*mode2*(mode2 - 1) + 1/2*(mode1 - 1)*(mode1 - 2) + (mode1 - 1)*(mode2 - 1)))
      out$nodes2 <- sum(max(becent[mode]) - becent[mode])/((mode2 - 1)*(1/2*mode1*(mode1 - 1) + 1/2 * (mode2 - 1) * (mode2 - 2) + (mode2 - 1) * (mode1 - 1)))
      if (mode1 > mode2) {
        out$nodes1 <- sum(max(becent[!mode]) - becent[!mode]) / (2 * (mode1 - 1)^2 * (mode2 - 1))
      }
      if (mode2 > mode1) {
        out$nodes2 <- sum(max(becent[mode]) - becent[mode]) / (2 * (mode2 - 1)^2 * (mode1 - 1))
      }
    }
    out <- c("Mode 1" = out$nodes1, "Mode 2" = out$nodes2)
  } else {
    out <- igraph::centr_betw(graph = graph)$centralization
  }
  out <- make_graph_measure(out, object)
  out
}

#' @describeIn centralisation Calculate the eigenvector centralization for a graph
#' @examples
#' graph_eigenvector(mpn_elite_mex)
#' graph_eigenvector(ison_southern_women)
#' @export
graph_eigenvector <- function(object, normalized = TRUE){
  if (is_twomode(object)) {
    out <- c(igraph::centr_eigen(as_igraph(to_mode1(object)), 
                                 normalized = normalized)$centralization,
             igraph::centr_eigen(as_igraph(to_mode2(object)), 
                                 normalized = normalized)$centralization)
  } else {
    out <- igraph::centr_eigen(as_igraph(object), 
                               normalized = normalized)$centralization
  }
  out <- make_graph_measure(out, object)
  out
}

