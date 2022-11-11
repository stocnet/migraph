#' Measures of network diversity
#' 
#' These functions offer ways to summarise the heterogeneity of an attribute
#' across a network, within groups of a network, or the distribution of ties
#' across this attribute.
#' @inheritParams as
#' @param attribute Name of a nodal attribute or membership vector
#'   to use as categories for the diversity measure.
#' @param clusters A nodal cluster membership vector or name of a vertex attribute.
#' @name diversity
#' @family measures
NULL

#' @describeIn diversity Calculates the number of unique categories 
#'   in a network attribute.
#' @export
network_richness <- function(object, attribute){
  make_network_measure(length(unique(node_attribute(object, attribute))),
                       object)
}

#' @describeIn diversity Calculates the number of unique categories 
#'   of an attribute to which each node is connected.
#' @export
node_richness <- function(object, attribute){
  out <- vapply(to_egos(object, min_dist = 1), 
         function(x) length(unique(node_attribute(x, attribute))),
         FUN.VALUE = numeric(1))
  make_node_measure(out, object)
}

#' @describeIn diversity Calculates the heterogeneity of ties across a network or 
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
#' marvel_friends <- to_unsigned(ison_marvel_relationships, "positive")
#' network_diversity(marvel_friends, "Gender")
#' network_diversity(marvel_friends, "Attractive")
#' network_diversity(marvel_friends, "Gender", "Rich")
#' @export
network_diversity <- function(object, attribute, clusters = NULL){
  blau <- function(features) { 1 - sum((table(features)/length(features))^2) }
  attr <- node_attribute(object, attribute)
  if (is.null(clusters)) {
    blauout <- blau(attr)
  } else if (is.numeric(clusters) && is.vector(clusters)) {
    blauout <- vapply(unique(clusters), 
                      function(i) blau(attr[clusters == i]),
                      numeric(1))
    names(blauout) <- paste0("Cluster ", unique(clusters))
  } else if (is.character(clusters)) {
    clu <- node_attribute(object, clusters)
    blauout <- vapply(unique(clu), 
                      function(i) blau(attr[clu == i]),
                      numeric(1))
    names(blauout) <- paste0("Cluster ", unique(clu))
    blauout <- blauout[order(names(blauout))]
  } else stop("`clusters` must be the name of a nodal variable in the object.")
  make_network_measure(blauout, object)
}

#' @describeIn diversity Calculates the heterogeneity of each node's
#'   local neighbourhood.
#' @examples 
#' node_diversity(marvel_friends, "Gender")
#' node_diversity(marvel_friends, "Attractive")
#' @export
node_diversity <- function(object, attribute){
  out <- vapply(igraph::ego(as_igraph(object)),
                function(x) network_diversity(
                  igraph::induced_subgraph(as_igraph(object), x),
                  attribute),
                FUN.VALUE = numeric(1))
  make_node_measure(out, object)
}

#' @describeIn diversity Calculates how embedded nodes in the network
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
#' network_homophily(marvel_friends, "Gender")
#' network_homophily(marvel_friends, "Attractive")
#' @export
network_homophily <- function(object, attribute){
  m <- as_matrix(object)
  if (length(attribute) == 1 && is.character(attribute)) {
    attribute <- node_attribute(object, attribute)
  }
  if (is.character(attribute) | is.numeric(attribute)) {
    attribute <- as.factor(attribute)
  }
  same <- outer(attribute, attribute, "==")
  nInternal <- sum(m * same)
  nExternal <- sum(m) - nInternal
  ei <- (nExternal - nInternal) / sum(m)
  make_network_measure(ei, object)
}

#' @describeIn diversity Calculates each node's embeddedness within groups
#'    of nodes with the same attribute
#' @examples 
#' node_homophily(marvel_friends, "Gender")
#' node_homophily(marvel_friends, "Attractive")
#' @export
node_homophily <- function(object, attribute){
  out <- vapply(igraph::ego(as_igraph(object)),
         function(x) network_homophily(
           igraph::induced_subgraph(as_igraph(object), x),
           attribute),
         FUN.VALUE = numeric(1))
  make_node_measure(out, object)
}

#' @describeIn diversity Calculates the degree assortativity in a graph.
#' @importFrom igraph assortativity_degree
#' @examples 
#' network_assortativity(mpn_elite_mex)
#' @export
network_assortativity <- function(object){
  make_network_measure(igraph::assortativity_degree(as_igraph(object), 
                               directed = is_directed(object)),
                     object)
}
