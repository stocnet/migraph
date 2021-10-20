#' Triad census by nodes or clusters
#' @name census
#' @param object a migraph-consistent object
#' @importFrom igraph vcount graph.neighborhood delete_vertices triad_census
#' @examples 
#' node_triad_census(ison_coleman)
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
  out # This line says the function returns the output
}

#' @rdname census
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
  mat
}

#' @rdname census
#' @param clusters a vector of cluster assignment
#' @export
cluster_triad_census <- function(object, clusters) {
  triads <- node_triad_census(object)
  cluster_triad_mat <- matrix(nrow = max(clusters), ncol = ncol(triads))
  for (i in seq_len(max(clusters))) {
    for (j in seq_len(ncol(triads))) {
      cluster_triad_mat[i, j] <- mean(triads[which(clusters == i), j])
    }
  }
  colnames(cluster_triad_mat) <- c("003", "012", "102", "021D",
                                   "021U", "021C", "111D", "111U",
                                   "030T", "030C", "201", "120D",
                                   "120U", "120C", "210", "300")
  cluster_triad_mat 
}

#' Triad census for the whole graph
#' @param object a migraph-consistent object
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