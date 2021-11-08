#' Measures of network diversity
#' @inheritParams as_igraph
#' @param attribute The name of a vertex attribute to measure the diversity of.
#' @param clusters A nodal cluster membership vector.
#' @name diversity
NULL

#' @rdname diversity
#' @export
graph_blau_index <- function(object, attribute, clusters = NULL){
  
  blau <- function (features) { 1-sum((table(features)/length(features))^2) }
  if(is.null(clusters)) clusters <- rep(1, graph_nodes(object))
  if(!is.numeric(clusters)) clusters <- node_attribute(object, clusters)

  unname(by(data = node_attribute(object, attribute), 
     INDICES = clusters, 
     FUN = blau))
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
