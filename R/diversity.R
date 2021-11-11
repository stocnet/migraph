#' Measures of network diversity
#' @inheritParams as_igraph
#' @param attribute The name of a vertex attribute to measure the diversity of.
#' @param clusters A nodal cluster membership vector.
#' @name diversity
NULL

#' @rdname diversity
#' @export
graph_blau_index <- function(object, attribute, clusters = NULL){
  blau <- function(features) { 1 - sum((table(features)/length(features))^2) }
  if (is.null(clusters)) {
    blauout <- blau(node_attribute(object, attribute))
  }
  if (!is.null(clusters) & !is.numeric(clusters)) {
    object <- as_igraph(object)
    V(object)$clusters <- node_attribute(object, clusters)
    blauout <- list()
    for (i in c(1:max(V(object)$clusters))) {
      currentcluster <- igraph::delete.vertices(object, V(object)[clusters != i])
      blauout[[i]] <- blau(node_attribute(currentcluster, attribute))
    }
    names(blauout) <- paste0("Cluster ", c(1:max(V(object)$clusters)))
  }
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
