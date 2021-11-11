#' Measures of network diversity
#' @inheritParams as_igraph
#' @param attribute The name of a vertex attribute to measure the diversity of.
#' @param clusters A nodal cluster membership vector.
#' @name diversity
NULL

#' @rdname diversity
#' @examples
#' graph_blau_index(ison_marvel_relationships, "Gender")
#' graph_blau_index(ison_marvel_relationships, "Attractive")
#' graph_blau_index(ison_marvel_relationships, "Gender", "Rich")
#' @export
graph_blau_index <- function(object, attribute, clusters = NULL){
  blau <- function(features) { 1 - sum((table(features)/length(features))^2) }
  attr <- node_attribute(object, attribute)
  if (is.null(clusters)) {
    blauout <- blau(attr)
  } else if (is.numeric(clusters) && is.vector(clusters)){
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
  } else stop("`clusters` must be the name of a nodal variable in the object.")
  blauout
}

#' @rdname diversity
#' @export
graph_ei_index <- function(object, attribute){
  m <- as_matrix(object)
  if(length(attribute) == 1 && is.character(attribute)) attribute <- node_attribute(object, attribute)
  if(is.character(attribute)) attribute <- as.factor(attribute)
  same <- outer(attribute, attribute, "==")
  nInternal <- sum(m * same)
  nExternal <- sum(m) - nInternal
  ei <- (nExternal - nInternal) / sum(m)
  ei
}
