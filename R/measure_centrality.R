# Degree-like centralities ####

#' Measures of degree-like centrality and centralisation
#'
#' @description
#'   These functions calculate common degree-related centrality measures for one- and two-mode networks:
#'   
#'   - `node_degree()` measures the degree centrality of nodes in an unweighted network,
#'   or weighted degree/strength of nodes in a weighted network; 
#'   there are several related shortcut functions:
#'     - `node_deg()` returns the unnormalised results.
#'     - `node_indegree()` returns the `direction = 'out'` results.
#'     - `node_outdegree()` returns the `direction = 'out'` results.
#'   - `node_multidegree()` measures the ratio between types of ties in a multiplex network.
#'   - `node_posneg()` measures the PN (positive-negative) centrality of a signed network.
#'   - `tie_degree()` measures the degree centrality of ties in a network
#'   - `network_degree()` measures a network's degree centralization; 
#'   there are several related shortcut functions:
#'     - `network_indegree()` returns the `direction = 'out'` results.
#'     - `network_outdegree()` returns the `direction = 'out'` results.
#'   
#'   All measures attempt to use as much information as they are offered,
#'   including whether the networks are directed, weighted, or multimodal.
#'   If this would produce unintended results, 
#'   first transform the salient properties using e.g. [to_undirected()] functions.
#'   All centrality and centralization measures return normalized measures by default,
#'   including for two-mode networks.
#' @name degree_centrality
#' @family centrality
#' @family measures
#' @seealso [to_undirected()] for removing edge directions
#'   and [to_unweighted()] for removing weights from a graph.
#' @inheritParams cohesion
#' @param normalized Logical scalar, whether the centrality scores are normalized.
#'   Different denominators are used depending on whether the object is one-mode or two-mode,
#'   the type of centrality, and other arguments.
#' @param alpha Numeric scalar, the positive tuning parameter introduced in
#'   Opsahl et al (2010) for trading off between degree and strength centrality measures.
#'   By default, `alpha = 0`, which ignores tie weights and the measure is solely based
#'   upon degree (the number of ties).
#'   `alpha = 1` ignores the number of ties and provides the sum of the tie weights 
#'   as strength centrality.
#'   Values between 0 and 1 reflect different trade-offs in the relative contributions of
#'   degree and strength to the final outcome, with 0.5 as the middle ground.
#'   Values above 1 penalise for the number of ties.
#'   Of two nodes with the same sum of tie weights, the node with fewer ties will obtain
#'   the higher score.
#'   This argument is ignored except in the case of a weighted network.
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
#' 
#' Opsahl, Tore, Filip Agneessens, and John Skvoretz. 2010. 
#' "Node centrality in weighted networks: Generalizing degree and shortest paths." 
#' _Social Networks_ 32, 245-251.
#' \doi{10.1016/j.socnet.2010.03.006}
#' @examples
#' node_degree(mpn_elite_mex)
#' node_degree(ison_southern_women)
#' @return Depending on how and what kind of an object is passed to the function,
#' the function will return a `tidygraph` object where the nodes have been updated
NULL

#' @rdname degree_centrality 
#' @importFrom manynet as_igraph
#' @export
node_degree <- function (.data, normalized = TRUE, alpha = 0,
                         direction = c("all","out","in")){
  
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  graph <- manynet::as_igraph(.data)
  weights <- `if`(manynet::is_weighted(.data), 
                  manynet::tie_weights(.data), NA)
  direction <- match.arg(direction)
  
  # Do the calculations
  if (manynet::is_twomode(graph) & normalized){
    degrees <- igraph::degree(graph = graph, 
                              v = igraph::V(graph), 
                              mode = direction, 
                              loops = manynet::is_complex(.data))
    other_set_size <- ifelse(igraph::V(graph)$type, 
                             sum(!igraph::V(graph)$type), 
                             sum(igraph::V(graph)$type))
    out <- degrees/other_set_size
  } else {
    if (all(is.na(weights))) {
      out <- igraph::degree(graph = graph, v = igraph::V(graph), 
                     mode = direction, 
                     loops = manynet::is_complex(.data),
                     normalized = normalized)
    }
    else {
      ki <- igraph::degree(graph = graph, v = igraph::V(graph), 
                     mode = direction, 
                     loops = manynet::is_complex(.data))
      si <- igraph::strength(graph = graph, vids = igraph::V(graph), 
                       mode = direction,
                       loops = manynet::is_complex(.data), weights = weights)
      out <- ki * (si/ki)^alpha
      if(normalized) out <- out/max(out)
    }
  }
  out <- make_node_measure(out, .data)
  out
}

#' @rdname degree_centrality
#' @export
node_deg <- function (.data, alpha = 0, direction = c("all","out","in")){
  node_degree(.data, normalized = FALSE, alpha = alpha, direction = direction)
}

#' @rdname degree_centrality
#' @export
node_outdegree <- function (.data, normalized = TRUE, alpha = 0){
  node_degree(.data, normalized = normalized, alpha = alpha, direction = "out")
}

#' @rdname degree_centrality
#' @export
node_indegree <- function (.data, normalized = TRUE, alpha = 0){
  node_degree(.data, normalized = normalized, alpha = alpha, direction = "in")
}

#' @rdname degree_centrality
#' @param tie1 Character string indicating the first uniplex network.
#' @param tie2 Character string indicating the second uniplex network.
#' @export
node_multidegree <- function (.data, tie1, tie2){
  stopifnot(manynet::is_multiplex(.data))
  out <- node_degree(manynet::to_uniplex(.data, tie1)) - 
    node_degree(manynet::to_uniplex(.data, tie2))
  make_node_measure(out, .data)
}

#' @rdname degree_centrality
#' @references
#' Everett, Martin G., and Stephen P. Borgatti. 2014. 
#' “Networks Containing Negative Ties.” 
#' _Social Networks_ 38:111–20. 
#' \doi{10.1016/j.socnet.2014.03.005}.
#' @export
node_posneg <- function(.data){
  stopifnot(manynet::is_signed(.data))
  pos <- manynet::as_matrix(manynet::to_unsigned(.data, keep = "positive"))
  neg <- manynet::as_matrix(manynet::to_unsigned(.data, keep = "negative"))
  nn <- manynet::network_nodes(.data)
  pn <- pos-neg*2
  diag(pn) <- 0
  idmat <- diag(nn)
  v1 <- matrix(1,nn,1)
  out <- solve(idmat - ((pn%*%t(pn))/(4*(nn-1)^2))) %*% (idmat+( pn/(2*(nn-1)) )) %*% v1
  make_node_measure(out, .data)
}

#' @rdname degree_centrality
#' @examples 
#' tie_degree(ison_adolescents)
#' @export
tie_degree <- function(.data, normalized = TRUE){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  edge_adj <- manynet::to_ties(.data)
  out <- node_degree(edge_adj, normalized = normalized)
  class(out) <- "numeric"
  out <- make_tie_measure(out, .data)
  out
}

#' @rdname degree_centrality
#' @examples
#' network_degree(ison_southern_women, direction = "in")
#' @export
network_degree <- function(.data, normalized = TRUE,
                           direction = c("all", "out", "in")){
  
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  direction <- match.arg(direction)
  
  if (manynet::is_twomode(.data)) {
    mat <- manynet::as_matrix(.data)
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
    out <- igraph::centr_degree(graph = .data, mode = direction, 
                                normalized = normalized)$centralization
  }
  out <- make_network_measure(out, .data)
  out
}

#' @rdname degree_centrality
#' @export
network_outdegree <- function(.data, normalized = TRUE){
  network_degree(.data, normalized = normalized, direction = "out")
}

#' @rdname degree_centrality
#' @export
network_indegree <- function(.data, normalized = TRUE){
  network_degree(.data, normalized = normalized, direction = "in")
}

# Betweenness-like centralities ####

#' Measures of betweenness-like centrality and centralisation
#' @description
#'   These functions calculate common betweenness-related centrality measures for one- and two-mode networks:
#'   
#'   - `node_betweenness()` measures the betweenness centralities of nodes in a network.
#'   - `node_induced()` measures the induced betweenness centralities of nodes in a network.
#'   - `node_flow()` measures the flow betweenness centralities of nodes in a network,
#'   which uses an electrical current model for information spreading 
#'   in contrast to the shortest paths model used by normal betweenness centrality.
#'   - `tie_betweenness()` measures the number of shortest paths going through a tie.
#'   - `network_betweenness()` measures the betweenness centralization for a network.
#'   
#'   All measures attempt to use as much information as they are offered,
#'   including whether the networks are directed, weighted, or multimodal.
#'   If this would produce unintended results, 
#'   first transform the salient properties using e.g. [to_undirected()] functions.
#'   All centrality and centralization measures return normalized measures by default,
#'   including for two-mode networks.
#' @name between_centrality
#' @family centrality
#' @family measures
#' @inheritParams degree_centrality
#' @param cutoff The maximum path length to consider when calculating betweenness.
#'   If negative or NULL (the default), there's no limit to the path lengths considered.
NULL

#' @rdname between_centrality
#' @import tidygraph
#' @examples
#' node_betweenness(mpn_elite_mex)
#' node_betweenness(ison_southern_women)
#' @return A numeric vector giving the betweenness centrality measure of each node.
#' @export 
node_betweenness <- function(.data, normalized = TRUE, 
                             cutoff = NULL){
  
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  weights <- `if`(manynet::is_weighted(.data), 
                  manynet::tie_weights(.data), NA)
  graph <- manynet::as_igraph(.data)
  
  # Do the calculations
  if (manynet::is_twomode(graph) & normalized){
    betw_scores <- igraph::betweenness(graph = graph, v = igraph::V(graph), 
                                       directed = manynet::is_directed(graph))
    other_set_size <- ifelse(igraph::V(graph)$type, sum(!igraph::V(graph)$type), sum(igraph::V(graph)$type))
    set_size <- ifelse(igraph::V(graph)$type, sum(igraph::V(graph)$type), sum(!igraph::V(graph)$type))
    out <- ifelse(set_size > other_set_size, 
                  betw_scores/(2*(set_size-1)*(other_set_size-1)), 
                  betw_scores/(1/2*other_set_size*(other_set_size-1)+1/2*(set_size-1)*(set_size-2)+(set_size-1)*(other_set_size-1)))
  } else {
    if (is.null(cutoff)) {
      out <- igraph::betweenness(graph = graph, v = igraph::V(graph), 
                                 directed = manynet::is_directed(graph), weights = weights, 
                                 normalized = normalized)
    } else {
      out <- igraph::betweenness(graph = graph, v = igraph::V(graph), 
                                 directed = manynet::is_directed(graph), 
                                 cutoff = cutoff, 
                                 weights = weights)
    }
  }
  out <- make_node_measure(out, .data)
  out
}

#' @rdname between_centrality 
#' @examples
#' node_induced(mpn_elite_mex)
#' @references
#' Everett, Martin and Steve Borgatti. 2010.
#' "Induced, endogenous and exogenous centrality"
#' _Social Networks_, 32: 339-344.
#' \doi{10.1016/j.socnet.2010.06.004}
#' @export 
node_induced <- function(.data, normalized = TRUE, 
                         cutoff = NULL){
  endog <- sum(node_betweenness(.data, normalized = normalized, cutoff = cutoff),
               na.rm = TRUE)
  exog <- vapply(seq.int(manynet::network_nodes(.data)),
                 function(x) sum(node_betweenness(manynet::delete_nodes(.data, x),
                                              normalized = normalized, cutoff = cutoff),
                                 na.rm = TRUE),
                FUN.VALUE = numeric(1))
  out <- endog - exog
  make_node_measure(out, .data)
}


#' @rdname between_centrality 
#' @importFrom sna flowbet
#' @export 
node_flow <- function(.data, normalized = TRUE){
  out <- sna::flowbet(manynet::as_network(.data),
                      gmode = ifelse(manynet::is_directed(.data), "digraph", "graph"),
                      diag = manynet::is_complex(.data),
                      cmode = ifelse(normalized, "normflow", "rawflow"))
  make_node_measure(out, .data)
}

#' @rdname between_centrality
#' @importFrom igraph edge_betweenness
#' @examples
#' (tb <- tie_betweenness(ison_adolescents))
#' plot(tb)
#' #ison_adolescents %>% mutate_ties(weight = tb) %>% 
#' #   autographr()
#' @export
tie_betweenness <- function(.data, normalized = TRUE){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  .data <- manynet::as_igraph(.data)
  eddies <- manynet::as_edgelist(.data)
  eddies <- paste(eddies[["from"]], eddies[["to"]], sep = "-")
  out <- igraph::edge_betweenness(.data)
  names(out) <- eddies
  out <- make_tie_measure(out, .data)
  out
}

#' @rdname between_centrality
#' @examples
#' network_betweenness(ison_southern_women, direction = "in")
#' @export
network_betweenness <- function(.data, normalized = TRUE,
                                direction = c("all", "out", "in")) {
  
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  direction <- match.arg(direction)
  graph <- manynet::as_igraph(.data)
  
  if (manynet::is_twomode(.data)) {
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
  out <- make_network_measure(out, .data)
  out
}

# Closeness-like centralities ####

#' Measures of closeness-like centrality and centralisation
#' @description
#'   These functions calculate common closeness-related centrality measures for one- and two-mode networks:
#'   
#'   - `node_closeness()` measures the closeness centrality of nodes in a network.
#'   - `node_reach()` measures nodes' reach centrality,
#'   or how many nodes they can reach within _k_ steps.
#'   - `node_harmonic()` measures nodes' harmonic centrality or valued centrality,
#'   which is thought to behave better than reach centrality for disconnected networks.
#'   - `node_information()` measures nodes' information centrality or 
#'   current-flow closeness centrality.
#'   - `tie_closeness()` measures the closeness of each tie to other ties in the network.
#'   - `network_closeness()` measures a network's closeness centralization.
#'   - `network_reach()` measures a network's reach centralization.
#'   - `network_harmonic()` measures a network's harmonic centralization.
#'   
#'   All measures attempt to use as much information as they are offered,
#'   including whether the networks are directed, weighted, or multimodal.
#'   If this would produce unintended results, 
#'   first transform the salient properties using e.g. [to_undirected()] functions.
#'   All centrality and centralization measures return normalized measures by default,
#'   including for two-mode networks.
#' @name close_centrality
#' @family centrality
#' @family measures
#' @inheritParams degree_centrality
NULL

#' @rdname close_centrality
#' @param cutoff Maximum path length to use during calculations.
#' @import tidygraph
#' @importFrom rlang %||%
#' @examples
#' node_closeness(mpn_elite_mex)
#' node_closeness(ison_southern_women)
#' @export
node_closeness <- function(.data, normalized = TRUE, 
                           direction = "out", cutoff = NULL){
  
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  weights <- `if`(manynet::is_weighted(.data), 
                  manynet::tie_weights(.data), NA)
  graph <- manynet::as_igraph(.data)
  
  # Do the calculations
  if (manynet::is_twomode(graph) & normalized){
    # farness <- rowSums(igraph::distances(graph = graph))
    closeness <- igraph::closeness(graph = graph, vids = igraph::V(graph), mode = direction)
    other_set_size <- ifelse(igraph::V(graph)$type, sum(!igraph::V(graph)$type), sum(igraph::V(graph)$type))
    set_size <- ifelse(igraph::V(graph)$type, sum(igraph::V(graph)$type), sum(!igraph::V(graph)$type))
    out <- closeness/(1/(other_set_size+2*set_size-2))
    } else {
      cutoff <- cutoff %||% -1
      out <- igraph::closeness(graph = graph, vids = igraph::V(graph), mode = direction, 
                               cutoff = cutoff, weights = weights, normalized = normalized)
    }
  out <- make_node_measure(out, .data)
  out
} 

#' @rdname close_centrality 
#' @param k Integer of steps out to calculate reach
#' @examples
#' node_reach(ison_adolescents)
#' @export
node_reach <- function(.data, normalized = TRUE, k = 2){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  out <- rowSums(node_path_census(.data)<=k)
  if(normalized) out <- out/(manynet::network_nodes(.data)-1)
  out <- make_node_measure(out, .data)
  out
}

#' @rdname close_centrality 
#' @references
#'   Marchiori, M, and V Latora. 2000. 
#'   "Harmony in the small-world".
#'   _Physica A_ 285: 539-546.
#'   
#'   Dekker, Anthony. 2005.
#'   "Conceptual distance in social network analysis".
#'   _Journal of Social Structure_ 6(3).
#' @export
node_harmonic <- function(.data, normalized = TRUE, k = -1){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  out <- igraph::harmonic_centrality(as_igraph(.data), # weighted if present
                                     normalized = normalized, cutoff = k)
  out <- make_node_measure(out, .data)
  out
}

#' @rdname close_centrality 
#' @importFrom sna infocent
#' @export
node_information <- function(.data, normalized = TRUE){
  out <- sna::infocent(manynet::as_network(.data),
                       gmode = ifelse(manynet::is_directed(.data), "digraph", "graph"),
                       diag = manynet::is_complex(.data))
  make_node_measure(out, .data)
}
  
#' @rdname close_centrality 
#' @examples
#' (ec <- tie_closeness(ison_adolescents))
#' plot(ec)
#' #ison_adolescents %>% 
#' #   activate(edges) %>% mutate(weight = ec) %>% 
#' #   autographr()
#' @export
tie_closeness <- function(.data, normalized = TRUE){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  edge_adj <- manynet::to_ties(.data)
  out <- node_closeness(edge_adj, normalized = normalized)
  class(out) <- "numeric"
  out <- make_tie_measure(out, .data)
  out
}

#' @rdname close_centrality 
#' @examples
#' network_closeness(ison_southern_women, direction = "in")
#' @export
network_closeness <- function(.data, normalized = TRUE,
                              direction = c("all", "out", "in")){
  
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  direction <- match.arg(direction)
  graph <- manynet::as_igraph(.data)
  
  if (manynet::is_twomode(.data)) {
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
  out <- make_network_measure(out, .data)
  out
}

#' @rdname close_centrality 
#' @export
network_reach <- function(.data, normalized = TRUE, k = 2){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  reaches <- node_reach(.data, normalized = FALSE, k = k)
  out <- sum(max(reaches) - reaches)
  if(normalized) out <- out / sum(manynet::network_nodes(.data) - reaches)
  make_network_measure(out, .data)
}

#' @rdname close_centrality
#' @export
network_harmonic <- function(.data, normalized = TRUE, k = 2){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  harm <- node_harmonic(.data, normalized = FALSE, k = k)
  out <- sum(max(harm) - harm)
  if(normalized) out <- out / sum(manynet::network_nodes(.data) - harm)
  make_network_measure(out, .data)
}

# Eigenvector-like centralities ####

#' Measures of eigenvector-like centrality and centralisation
#' @description
#'   These functions calculate common eigenvector-related centrality measures for one- and two-mode networks:
#'   
#'   - `node_eigenvector()` measures the eigenvector centrality of nodes in a network.
#'   - `node_power()` measures the Bonacich, beta, or power centrality of nodes in a network.
#'   - `node_alpha()` measures the alpha or Katz centrality of nodes in a network.
#'   - `node_pagerank()` measures the pagerank centrality of nodes in a network.
#'   - `tie_eigenvector()` measures the eigenvector centrality of ties in a network.
#'   - `network_eigenvector()` measures the eigenvector centralization for a network.
#'   
#'   All measures attempt to use as much information as they are offered,
#'   including whether the networks are directed, weighted, or multimodal.
#'   If this would produce unintended results, 
#'   first transform the salient properties using e.g. [to_undirected()] functions.
#'   All centrality and centralization measures return normalized measures by default,
#'   including for two-mode networks.
#' @name eigenv_centrality
#' @family centrality
#' @family measures
#' @inheritParams degree_centrality
NULL

#' @rdname eigenv_centrality
#' @section Eigenvector centrality:
#'   Eigenvector centrality operates as a measure of a node's influence in a network.
#'   The idea is that being connected to well-connected others results in a higher score.
#'   Each node's eigenvector centrality can be defined as:
#'   \deqn{x_i = \frac{1}{\lambda} \sum_{j \in N} a_{i,j} x_j}
#'   where \eqn{a_{i,j} = 1} if \eqn{i} is linked to \eqn{j} and 0 otherwise,
#'   and \eqn{\lambda} is a constant representing the principal eigenvalue.
#'   Rather than performing this iteration,
#'   most routines solve the eigenvector equation \eqn{Ax = \lambda x}.
#' @param scale Logical scalar, whether to rescale the vector so the maximum score is 1. 
#' @details
#'   We use `{igraph}` routines behind the scenes here for consistency and because they are often faster.
#'   For example, `igraph::eigencentrality()` is approximately 25% faster than `sna::evcent()`.
#' @references 
#'   Bonacich, Phillip. 1991. 
#'   “Simultaneous Group and Individual Centralities.” 
#'   _Social Networks_ 13(2):155–68. 
#'   \doi{10.1016/0378-8733(91)90018-O}.
#' @examples
#' node_eigenvector(mpn_elite_mex)
#' node_eigenvector(ison_southern_women)
#' @return A numeric vector giving the eigenvector centrality measure of each node.
#' @export 
node_eigenvector <- function(.data, normalized = TRUE, scale = FALSE){
  
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  weights <- `if`(manynet::is_weighted(.data), 
                  manynet::tie_weights(.data), NA)
  graph <- manynet::as_igraph(.data)
  
  if(!manynet::is_connected(.data)) 
    warning("Unconnected networks will only allow nodes from one component have non-zero eigenvector scores.")
  
  # Do the calculations
  if (!manynet::is_twomode(graph)){
    out <- igraph::eigen_centrality(graph = graph, 
                                    directed = manynet::is_directed(graph), scale = scale, 
                                    options = igraph::arpack_defaults())$vector
    if (normalized) out <- out / sqrt(1/2)
    if(scale) out <- out / max(out)
  } else {
    eigen1 <- manynet::to_mode1(graph)
    eigen1 <- igraph::eigen_centrality(graph = eigen1, 
                                       directed = manynet::is_directed(eigen1), scale = scale, 
                                       options = igraph::arpack_defaults())$vector
    eigen2 <- manynet::to_mode2(graph)
    eigen2 <- igraph::eigen_centrality(graph = eigen2, 
                                       directed = manynet::is_directed(eigen2), scale = scale, 
                                       options = igraph::arpack_defaults())$vector
    out <- c(eigen1, eigen2)
    if (normalized) out <- out / sqrt(1/2)
    if(scale) out <- out / max(out)
  }
  out <- make_node_measure(out, .data)
  out
}

#' @rdname eigenv_centrality
#' @param exponent Decay rate for the Bonacich power centrality score.
#' @section Power centrality:
#'   Power or beta (or Bonacich) centrality 
#' @references 
#'   Bonacich, Phillip. 1987. 
#'   “Power and Centrality: A Family of Measures.” 
#'   _The American Journal of Sociology_, 92(5): 1170–82.
#' \doi{10.1086/228631}.
#' @importFrom igraph power_centrality
#' @examples
#' node_power(ison_southern_women, exponent = 0.5)
#' @return A numeric vector giving each node's power centrality measure.
#' @export 
node_power <- function(.data, normalized = TRUE, scale = FALSE, exponent = 1){
  
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  weights <- `if`(manynet::is_weighted(.data), 
                  manynet::tie_weights(.data), NA)
  graph <- manynet::as_igraph(.data)
  
  # Do the calculations
  if (!manynet::is_twomode(graph)){
    out <- igraph::power_centrality(graph = graph, 
                                    exponent = exponent,
                                    rescale = scale)
    if (normalized) out <- out / sqrt(1/2)
  } else {
    eigen1 <- manynet::to_mode1(graph)
    eigen1 <- igraph::power_centrality(graph = eigen1, 
                                       exponent = exponent,
                                       rescale = scale)
    eigen2 <- manynet::to_mode2(graph)
    eigen2 <- igraph::power_centrality(graph = eigen2, 
                                       exponent = exponent,
                                       rescale = scale)
    out <- c(eigen1, eigen2)
    if (normalized) out <- out / sqrt(1/2)
  }
  out <- make_node_measure(out, .data)
  out
}

#' @rdname eigenv_centrality 
#' @param alpha A constant that trades off the importance of external influence against the importance of connection.
#'   When \eqn{\alpha = 0}, only the external influence matters.
#'   As \eqn{\alpha} gets larger, only the connectivity matters and we reduce to eigenvector centrality.
#'   By default \eqn{\alpha = 0.85}.
#' @section Alpha centrality:
#'   Alpha or Katz (or Katz-Bonacich) centrality operates better than eigenvector centrality
#'   for directed networks.
#'   Eigenvector centrality will return 0s for all nodes not in the main strongly-connected component.
#'   Each node's alpha centrality can be defined as:
#'   \deqn{x_i = \frac{1}{\lambda} \sum_{j \in N} a_{i,j} x_j + e_i}
#'   where \eqn{a_{i,j} = 1} if \eqn{i} is linked to \eqn{j} and 0 otherwise,
#'   \eqn{\lambda} is a constant representing the principal eigenvalue,
#'   and \eqn{e_i} is some external influence used to ensure that even nodes beyond the main
#'   strongly connected component begin with some basic influence.
#'   Note that many equations replace \eqn{\frac{1}{\lambda}} with \eqn{\alpha},
#'   hence the name.
#'
#'   For example, if \eqn{\alpha = 0.5}, then each direct connection (or alter) would be worth \eqn{(0.5)^1 = 0.5},
#'   each secondary connection (or tertius) would be worth \eqn{(0.5)^2 = 0.25},
#'   each tertiary connection would be worth \eqn{(0.5)^3 = 0.125}, and so on.
#'
#'   Rather than performing this iteration though,
#'   most routines solve the equation \eqn{x = (I - \frac{1}{\lambda} A^T)^{-1} e}.
#' @importFrom igraph alpha_centrality
#' @references 
#'   Katz, Leo 1953. 
#'   "A new status index derived from sociometric analysis". 
#'   _Psychometrika_. 18(1): 39–43.
#' 
#'   Bonacich, P. and Lloyd, P. 2001. 
#'   “Eigenvector-like measures of centrality for asymmetric relations” 
#'   _Social Networks_. 23(3):191-201.
#' @export 
node_alpha <- function(.data, alpha = 0.85){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  make_node_measure(igraph::alpha_centrality(manynet::as_igraph(.data), 
                                             alpha = alpha),
                    .data)
}

#' @rdname eigenv_centrality 
#' @references 
#'   Brin, Sergey and Page, Larry. 1998.
#'   "The anatomy of a large-scale hypertextual web search engine".
#'   _Proceedings of the 7th World-Wide Web Conference_. Brisbane, Australia.
#' @export 
node_pagerank <- function(.data){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  make_node_measure(igraph::page_rank(manynet::as_igraph(.data)),
                    .data)
}
  
#' @rdname eigenv_centrality
#' @examples 
#' tie_eigenvector(ison_adolescents)
#' @export
tie_eigenvector <- function(.data, normalized = TRUE){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  edge_adj <- manynet::to_ties(.data)
  out <- node_eigenvector(edge_adj, normalized = normalized)
  class(out) <- "numeric"
  out <- make_tie_measure(out, .data)
  out
}

#' @rdname eigenv_centrality 
#' @examples
#' network_eigenvector(mpn_elite_mex)
#' network_eigenvector(ison_southern_women)
#' @export
network_eigenvector <- function(.data, normalized = TRUE){
  if (manynet::is_twomode(.data)) {
    out <- c(igraph::centr_eigen(manynet::as_igraph(manynet::to_mode1(.data)), 
                                 normalized = normalized)$centralization,
             igraph::centr_eigen(manynet::as_igraph(manynet::to_mode2(.data)), 
                                 normalized = normalized)$centralization)
  } else {
    out <- igraph::centr_eigen(manynet::as_igraph(.data), 
                               normalized = normalized)$centralization
  }
  out <- make_network_measure(out, .data)
  out
}


