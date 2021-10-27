#' Census by nodes or clusters
#' 
#' These functions include ways to take a census of the positions of nodes
#' in a network. These include a triad census based on the triad profile
#' of nodes, but also a tie census based on the particular tie partners
#' of nodes. Included also are group census functions for summarising
#' the profiles of clusters of nodes in a network.
#' @name census
#' @param object a migraph-consistent object
#' @param clusters a vector of cluster assignment
#' @param decimals number of decimal points to round to
#' @importFrom igraph vcount graph.neighborhood delete_vertices triad_census
NULL

#' @rdname census
#' @examples
#' task_eg <- to_named(to_uniplex(ison_m182, "task_tie"))
#' (tie_cen <- node_tie_census(task_eg))
#' @export
node_tie_census <- function(object){
  object <- as_igraph(object)
  edge_names <- igraph::edge_attr_names(object)
  if (is_directed(object)) {
    mat <- vector()
    if (length(edge_names) > 0) {
      for (e in edge_names) {
        rc <- igraph::as_adjacency_matrix(object, attr = e, sparse = F)
        rccr <- rbind(rc, t(rc))
        mat <- rbind(mat, rccr)
      }} else {
        rc <- igraph::as_adjacency_matrix(object, sparse = F)
        rccr <- rbind(rc, t(rc))
        mat <- rbind(mat, rccr)
      }
  } else {
    mat <- vector() 
    if (length(edge_names) > 0) {
      for (e in edge_names) {
        rc <- igraph::as_adjacency_matrix(object, attr = e, sparse = F)
        mat <- rbind(mat, rc)
      }} else {
        rc <- igraph::as_adjacency_matrix(object, sparse = F)
        mat <- rbind(mat, rc)
      }
  }
  if(is_labelled(object) & is_directed(object)) rownames(mat) <- rep(c(paste0("from", igraph::V(object)$name),
                                                                   paste0("to", igraph::V(object)$name)),
                                                                  times = length(edge_names))
  t(mat)
}

#' @rdname census
#' @examples 
#' (triad_cen <- node_triad_census(task_eg))
#' @export
node_triad_census <- function(object){
  x <- as_igraph(object)
  out <- vector() # This line intialises an empty vector
  for (i in seq_len(igraph::vcount(x))) { # For each (i) in 
    nb.wi <- igraph::graph.neighborhood(x,
                                        order = 1,
                                        V(x)[i],
                                        mode = 'all')[[1]] 
    # Get i's local neighbourhood. See also make_ego_graph()
    nb.wi <- nb.wi + (igraph::vcount(x) - igraph::vcount(nb.wi)) 
    # Add the removed vertices back in (empty) for symmetry
    nb.wo <- igraph::delete_vertices(nb.wi, i)
    # Make a graph without i in it
    out <- rbind(out,
                 suppressWarnings(igraph::triad_census(nb.wi)) - 
                   suppressWarnings(igraph::triad_census(nb.wo)) )
    # Get the difference in triad census between the local graph
    # with and without i to get i's triad census
  } # Close the for loop
  colnames(out) <- c("003", "012", "102", "021D",
                     "021U", "021C", "111D", "111U",
                     "030T", "030C", "201", "120D",
                     "120U", "120C", "210", "300")
  if (!is_directed(object)) out <- out[, c(1, 2, 3, 11, 15, 16)]
  rownames(out) <- igraph::V(x)$name
  out # This line says the function returns the output
}

#' @rdname census
#' @examples 
#' group_tie_census(task_eg, cutree(cluster_structural_equivalence(task_eg), 4))
#' @export
group_tie_census <- function(object, clusters, decimals = 2) {
  ties <- node_tie_census(object)
  cluster_tie_mat <- matrix(nrow = max(clusters), ncol = ncol(ties))
  for (i in seq_len(max(clusters))) {
    for (j in seq_len(ncol(ties))) {
      cluster_tie_mat[i, j] <- round(mean(ties[which(clusters == i), j]), decimals)
    }
  }
  colnames(cluster_tie_mat) <- colnames(ties)
  if(is.numeric(clusters)){
    rownames(cluster_tie_mat) <- paste("Block", 1:max(clusters))
  } else {
    rownames(cluster_tie_mat) <- clusters
  }
  cluster_tie_mat
}

#' @rdname census
#' @examples 
#' group_triad_census(task_eg, cutree(cluster_regular_equivalence(task_eg), 4))
#' @export
group_triad_census <- function(object, clusters, decimals = 2) {
  triads <- node_triad_census(object)
  cluster_triad_mat <- matrix(nrow = max(clusters), ncol = ncol(triads))
  for (i in seq_len(max(clusters))) {
    for (j in seq_len(ncol(triads))) {
      cluster_triad_mat[i, j] <- round(mean(triads[which(clusters == i), j]), decimals)
    }
  }
  colnames(cluster_triad_mat) <- c("003", "012", "102", "021D",
                                   "021U", "021C", "111D", "111U",
                                   "030T", "030C", "201", "120D",
                                   "120U", "120C", "210", "300")
  if(is.numeric(clusters)){
    rownames(cluster_triad_mat) <- paste("Block", 1:max(clusters))
  } else {
    rownames(cluster_triad_mat) <- clusters
  }
  cluster_triad_mat 
}

#' @title Get triad census summary by group
#' `r lifecycle::badge("deprecated")`
#' @keywords internal
#' @description Deprecated on 2021-10-26.
#' @return `group_triad_census()`
#' @export
cluster_triad_census <- function(object, clusters) {
  .Deprecated("group_triad_census")
  group_triad_census(object, clusters)
}

#' Censuses for the whole graph
#' @name graph_census
#' @param object a migraph-consistent object
NULL

#' @rdname graph_census
#' @examples 
#' graph_dyad_census(ison_coleman)
#' @export
graph_dyad_census <- function(object) {
  if (is_twomode(object)) {
    stop("A twomode or multilevel option for a dyad census is not yet implemented.")
  } else {
    out <- suppressWarnings(igraph::dyad_census(as_igraph(object)))
    out <- unlist(out)
    names(out) <- c("Mutual", "Asymmetric", "Null")
    if (!is_directed(object)) out <- out[c(1, 3)]
    out
  }
}

#' @rdname graph_census
#' @examples 
#' graph_triad_census(ison_coleman)
#' @export
graph_triad_census <- function(object) {
  if (is_twomode(object)) {
    stop("A twomode or multilevel option for a triad census is not yet implemented.")
  } else {
    out <- suppressWarnings(igraph::triad_census(as_igraph(object)))
    names(out) <- c("003", "012", "102", "021D",
                    "021U", "021C", "111D", "111U",
                    "030T", "030C", "201", "120D",
                    "120U", "120C", "210", "300")
    if (!is_directed(object)) out <- out[c(1, 2, 3, 11, 15, 16)]
    out
  }
}