#' Logical whether nodes meet some criteria
#' 
#' These functions identify which nodes in a network meet some criteria,
#' returning a logical vector the length of the nodes in a network.
#' Such functions are often useful for identifying nodes 
#' that are particularly well- or poorly-connected.
#' @return TRUE if the condition is met, or FALSE otherwise.
#' @inheritParams is
#' @family marks
#' @name mark
NULL

#' @describeIn mark Returns logical of which nodes cut
#'   or act as articulation points in a network,
#'   increasing the number of connected components in a graph when removed.
#' @importFrom igraph articulation_points
#' @examples 
#' node_is_cutpoint(ison_brandes)
#' @export
node_is_cutpoint <- function(object){
  if(is_labelled(object)){
    out <- node_names(object) %in% 
      attr(igraph::articulation_points(as_igraph(object)), 
           "names")
    names(out) <- node_names(object)
  } else {
    out <- 1:graph_nodes(object) %in% 
      igraph::articulation_points(as_igraph(object))
  }
  make_node_mark(out, object)
}

#' @describeIn mark Returns logical of which nodes are isolates,
#'   with neither incoming nor outgoing ties.
#' @examples 
#' node_is_isolate(ison_brandes)
#' @export
node_is_isolate <- function(object){
  mat <- as_matrix(object)
  if(is_twomode(object)){
    out <- c(rowSums(mat)==0, colSums(mat)==0)
  } else {
    out <- rowSums(mat)==0 & colSums(mat)==0
  }
  names(out) <- node_names(object)
  make_node_mark(out, object)
}