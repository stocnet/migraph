#' Measures of network diversity
#' 
#' These functions offer ways to summarise the heterogeneity of an attribute
#' across a network, within groups of a network, or the distribution of ties
#' across this attribute.
#' @inheritParams as_igraph
#' @param attribute The name of a vertex attribute to measure the diversity of.
#' @param clusters A nodal cluster membership vector or name of a vertex attribute.
#' @name diversity
NULL

#' @rdname diversity
#' @examples
#' marvel_friends <- to_unsigned(ison_marvel_relationships, "positive")
#' graph_blau_index(marvel_friends, "Gender")
#' graph_blau_index(marvel_friends, "Attractive")
#' graph_blau_index(marvel_friends, "Gender", "Rich")
#' @export
graph_blau_index <- function(object, attribute, clusters = NULL){
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
  blauout
}

#' @rdname diversity
#' @examples 
#' graph_ei_index(marvel_friends, "Gender")
#' graph_ei_index(marvel_friends, "Attractive")
#' @export
graph_ei_index <- function(object, attribute){
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
  ei
}
