#' Measures of network diversity
#' 
#' These functions offer ways to summarise the heterogeneity of an attribute
#' across a network, within groups of a network, or the distribution of ties
#' across this attribute.
#' @inheritParams is
#' @param attribute Name of a nodal attribute or membership vector
#'   to use as categories for the diversity measure.
#' @param clusters A nodal cluster membership vector or name of a vertex attribute.
#' @name heterogeneity
#' @family measures
NULL

#' @describeIn heterogeneity Calculates the number of unique categories 
#'   in a network attribute.
#' @examples
#' network_richness(mpn_bristol)
#' @export
network_richness <- function(.data, attribute){
  make_network_measure(length(unique(manynet::node_attribute(.data, attribute))),
                       .data)
}

#' @describeIn heterogeneity Calculates the number of unique categories 
#'   of an attribute to which each node is connected.
#' @examples
#' node_richness(mpn_bristol, "type")
#' @export
node_richness <- function(.data, attribute){
  out <- vapply(manynet::to_egos(.data, min_dist = 1), 
         function(x) length(unique(manynet::node_attribute(x, attribute))),
         FUN.VALUE = numeric(1))
  make_node_measure(out, .data)
}

#' @describeIn heterogeneity Calculates the heterogeneity of ties across a network or 
#'    within clusters by node attributes.
#' @section network_diversity:
#'    Blau's index (1977) uses a formula known also in other disciplines
#'    by other names 
#'    (Gini-Simpson Index, Gini impurity, Gini's diversity index, 
#'    Gibbs-Martin index, and probability of interspecific encounter (PIE)): 
#'    \deqn{1 - \sum\limits_{i = 1}^k {p_i^2 }}, 
#'    where \eqn{p_i} is the proportion of group members in \eqn{i}th category 
#'    and \eqn{k} is the number of categories for an attribute of interest. 
#'    This index can be interpreted as the probability that two members 
#'    randomly selected from a group would be from different categories. 
#'    This index finds its minimum value (0) when there is no variety, 
#'    i.e. when all individuals are classified in the same category. 
#'    The maximum value depends on the number of categories and 
#'    whether nodes can be evenly distributed across categories. 
#' @references 
#'   Blau, Peter M. (1977). 
#'   _Inequality and heterogeneity_. 
#'   New York: Free Press.
#' @examples
#' marvel_friends <- manynet::to_unsigned(manynet::ison_marvel_relationships, "positive")
#' network_diversity(marvel_friends, "Gender")
#' network_diversity(marvel_friends, "Attractive")
#' network_diversity(marvel_friends, "Gender", "Rich")
#' @export
network_diversity <- function(.data, attribute, clusters = NULL){
  blau <- function(features) { 1 - sum((table(features)/length(features))^2) }
  attr <- manynet::node_attribute(.data, attribute)
  if (is.null(clusters)) {
    blauout <- blau(attr)
  } else if (is.numeric(clusters) && is.vector(clusters)) {
    blauout <- vapply(unique(clusters), 
                      function(i) blau(attr[clusters == i]),
                      numeric(1))
    names(blauout) <- paste0("Cluster ", unique(clusters))
  } else if (is.character(clusters)) {
    clu <- manynet::node_attribute(.data, clusters)
    blauout <- vapply(unique(clu), 
                      function(i) blau(attr[clu == i]),
                      numeric(1))
    names(blauout) <- paste0("Cluster ", unique(clu))
    blauout <- blauout[order(names(blauout))]
  } else stop("`clusters` must be the name of a nodal variable in the object.")
  make_network_measure(blauout, .data)
}

#' @describeIn heterogeneity Calculates the heterogeneity of each node's
#'   local neighbourhood.
#' @examples 
#' node_diversity(marvel_friends, "Gender")
#' node_diversity(marvel_friends, "Attractive")
#' @export
node_diversity <- function(.data, attribute){
  out <- vapply(igraph::ego(manynet::as_igraph(.data)),
                function(x) network_diversity(
                  igraph::induced_subgraph(manynet::as_igraph(.data), x),
                  attribute),
                FUN.VALUE = numeric(1))
  make_node_measure(out, .data)
}

#' @describeIn heterogeneity Calculates how embedded nodes in the network
#'    are within groups of nodes with the same attribute
#' @section network_homophily:
#'   Given a partition of a network into a number of mutually exclusive groups then 
#'   The E-I index is the number of ties between (or _external_) nodes 
#'   grouped in some mutually exclusive categories
#'   minus the number of ties within (or _internal_) these groups
#'   divided by the total number of ties. 
#'   This value can range from 1 to -1,
#'   where 1 indicates ties only between categories/groups and -1 ties only within categories/groups.
#' @references 
#'   Krackhardt, David and Robert N. Stern (1988). 
#'   Informal networks and organizational crises: an experimental simulation. 
#'   _Social Psychology Quarterly_ 51(2), 123-140.
#' @examples 
#' network_heterophily(marvel_friends, "Gender")
#' network_heterophily(marvel_friends, "Attractive")
#' @export
network_heterophily <- function(.data, attribute){
  m <- manynet::as_matrix(.data)
  if (length(attribute) == 1 && is.character(attribute)) {
    attribute <- manynet::node_attribute(.data, attribute)
  }
  if (is.character(attribute) | is.numeric(attribute)) {
    attribute <- as.factor(attribute)
  }
  same <- outer(attribute, attribute, "==")
  nInternal <- sum(m * same, na.rm = TRUE)
  nExternal <- sum(m, na.rm = TRUE) - nInternal
  ei <- (nExternal - nInternal) / sum(m, na.rm = TRUE)
  make_network_measure(ei, .data)
}

#' @describeIn heterogeneity Calculates each node's embeddedness within groups
#'    of nodes with the same attribute
#' @examples 
#' node_heterophily(marvel_friends, "Gender")
#' node_heterophily(marvel_friends, "Attractive")
#' @export
node_heterophily <- function(.data, attribute){
  m <- manynet::as_matrix(.data)
  if (length(attribute) == 1 && is.character(attribute)) {
    attribute <- manynet::node_attribute(.data, attribute)
  }
  if (is.character(attribute) | is.numeric(attribute)) {
    attribute <- as.factor(attribute)
  }
  if(anyNA(attribute)){
    m[is.na(attribute),] <- NA
    m[,is.na(attribute)] <- NA
  }
  same <- outer(attribute, attribute, "==")
  nInternal <- rowSums(m * same, na.rm = TRUE)
  nInternal[is.na(attribute)] <- NA
  nExternal <- rowSums(m, na.rm = TRUE) - nInternal
  ei <- (nExternal - nInternal) / rowSums(m, na.rm = TRUE)
  make_node_measure(ei, .data)
}

#' @describeIn heterogeneity Calculates the degree assortativity in a graph.
#' @importFrom igraph assortativity_degree
#' @examples 
#' network_assortativity(mpn_elite_mex)
#' @export
network_assortativity <- function(.data){
  make_network_measure(igraph::assortativity_degree(manynet::as_igraph(.data), 
                               directed = manynet::is_directed(.data)),
                     .data)
}
