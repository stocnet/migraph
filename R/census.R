#' Census by nodes or clusters
#' 
#' These functions include ways to take a census of the positions of nodes
#' in a network. These include a triad census based on the triad profile
#' of nodes, but also a tie census based on the particular tie partners
#' of nodes. Included also are group census functions for summarising
#' the profiles of clusters of nodes in a network.
#' @name census
#' @inheritParams is
#' @param clusters a vector of cluster assignment.
#' @param decimals Number of decimal points to round to.
#' @importFrom igraph vcount graph.neighborhood delete_vertices triad_census
NULL

#' @rdname census
#' @examples
#' task_eg <- to_named(to_uniplex(ison_algebra, "task_tie"))
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

#' @describeIn census Returns a census of nodes' positions
#'   in motifs of four nodes.
#' @details The quad census uses the `{oaqc}` package to do
#'   the heavy lifting of counting the number of each orbits.
#'   See `vignette('oaqc')`.
#'   However, our function relabels some of the motifs
#'   to avoid conflicts and improve some consistency with 
#'   other census-labelling practices.
#'   The letter-number pairing of these labels indicate
#'   the number and configuration of ties.
#'   For now, we offer a rough translation:
#'   
#' | migraph | Ortmann and Brandes      
#' | ------------- |------------- |
#' | E4  | co-K4
#' | I40, I41  | co-diamond
#' | H4  | co-C4
#' | L42, L41, L40 | co-paw
#' | D42, D40 | co-claw
#' | U42, U41 | P4
#' | Y43, Y41 | claw
#' | P43, P42, P41 | paw
#' | 04 | C4
#' | Z42, Z43 | diamond
#' | X4 | K4
#' @importFrom oaqc oaqc
#' @importFrom tidygraph %E>%
#' @references 
#'  Ortmann, Mark, and Ulrik Brandes. 2017. 
#'  “Efficient Orbit-Aware Triad and Quad Census in Directed and Undirected Graphs.” 
#'  \emph{Applied Network Science} 2(1):13. \doi{https://doi.org/10.1007/s41109-017-0027-2}
#' @examples 
#' (quad_cen <- node_quad_census(ison_southern_women))
#' @export
node_quad_census <- function(object){
  graph <- object %>% as_tidygraph() %E>% 
    as.data.frame()
  out <- oaqc::oaqc(graph)[[1]]
  out <- out[-1,]
  rownames(out) <- node_names(object)
  colnames(out) <- c("E4", # co-K4
                     "I41","I40", # co-diamond
                     "H4", # co-C4
                     "L42","L41","L40", # co-paw
                     "D42","D40", # co-claw
                     "U42","U41", # P4
                     "Y43","Y41", # claw
                     "P43","P42","P41", # paw
                     "04", # C4
                     "Z42","Z43", # diamond
                     "X4") # K4
  if(is_twomode(object)) out <- out[,-c(8,9,14,15,16,18,19,20)]
  out
}

#' Censuses for the whole graph
#' @name graph_census
#' @param object A migraph-consistent object.
#' @param object2 A second, two-mode migraph-consistent object.
NULL

#' @rdname graph_census
#' @references 
#' Hollway, James, Alessandro Lomi, Francesca Pallotti, and Christoph Stadtfeld. 
#' “\doi{10.1017/nws.2017.8}{Multilevel Social Spaces: The Network Dynamics of Organizational Fields}.” 
#' _Network Science_ 5, no. 2 (June 2017): 187–212.
#' @source Alejandro Espinosa 'netmem'
#' @examples 
#' marvel_friends <- to_unsigned(ison_marvel_relationships, "positive")
#' (mixed_cen <- graph_mixed_census(marvel_friends, ison_marvel_teams))
#' @export
graph_mixed_census <- function (object, object2) {
  if(is_twomode(object)) stop("First object should be a one-mode network")
  if(!is_twomode(object2)) stop("Second object should be a two-mode network")
  if(graph_dims(object)[1]!=graph_dims(object2)[1]) stop("Non-conformable arrays")
  
  m1 <- as_matrix(object)
  m2 <- as_matrix(object2)
  cp <- function(m) (-m + 1)
  
  onemode.reciprocal <- m1 * t(m1)
  onemode.forward <- m1 * cp(t(m1))
  onemode.backward <- cp(m1) * t(m1)
  onemode.null <- cp(m1) * cp(t(m1))
  diag(onemode.forward) <- 0
  diag(onemode.backward) <- 0
  diag(onemode.null) <- 0
  
  bipartite.twopath <- m2 %*% t(m2)
  bipartite.null <- cp(m2) %*% cp(t(m2))
  bipartite.onestep1 <- m2 %*% cp(t(m2))
  bipartite.onestep2 <- cp(m2) %*% t(m2)
  diag(bipartite.twopath) <- 0
  diag(bipartite.null) <- 0
  diag(bipartite.onestep1) <- 0
  diag(bipartite.onestep2) <- 0
  
  res <- c("22" = sum(onemode.reciprocal * bipartite.twopath) / 2,
           "21" = sum(onemode.forward * bipartite.twopath) / 2 + sum(onemode.backward * bipartite.twopath) / 2,
           "20" = sum(onemode.null * bipartite.twopath) / 2,
           "12" = sum(onemode.reciprocal * bipartite.onestep1) / 2 + sum(onemode.reciprocal * bipartite.onestep2) / 2,
           "11D" = sum(onemode.forward * bipartite.onestep1) / 2 + sum(onemode.backward * bipartite.onestep2) / 2,
           "11U" = sum(onemode.forward * bipartite.onestep2) / 2 + sum(onemode.backward * bipartite.onestep1) / 2,
           "10" = sum(onemode.null * bipartite.onestep2) / 2 + sum(onemode.null * bipartite.onestep1) / 2,
           "02" = sum(onemode.reciprocal * bipartite.null) / 2,
           "01" = sum(onemode.forward * bipartite.null) / 2 + sum(onemode.backward * bipartite.null) / 2,
           "00" = sum(onemode.null * bipartite.null) / 2)  
  return(res)
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

#' @rdname graph_census
#' @examples 
#' graph_dyad_census(ison_adolescents)
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
#' graph_triad_census(ison_adolescents)
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