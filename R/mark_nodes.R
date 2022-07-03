#' Marking nodes based on their properties
#' 
#' @description 
#'   These functions return logical vectors the length of the 
#'   nodes in a network identifying which hold certain properties.
#'   
#'   `node_is_cutpoint()` and `node_is_isolate()` are useful for identifying
#'   nodes that are in particular positions in the network.
#'   More can be added here.
#'   
#'   `node_is_max()` and `node_is_min()` are more generally useful
#'   for converting the results from some node measure into a mark-class object.
#'   They can be particularly useful for highlighting which node or nodes
#'   are key because they minimise or, more often, maximise some measure.
#' @inheritParams is
#' @family marks
#' @name mark_nodes
NULL

#' @describeIn mark_nodes Returns logical of which nodes cut
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

#' @describeIn mark_nodes Returns logical of which nodes are isolates,
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

#' @describeIn mark_nodes Returns logical of which nodes 
#'   hold the maximum of some measure
#' @param node_measure An object created by a `node_` measure.
#' @examples 
#' node_is_max(node_degree(ison_brandes))
#' @export
node_is_max <- function(node_measure){
  if(!inherits(node_measure, "node_measure"))
    stop("This function expects an object of class `node_measure`")
  if(any(attr(node_measure, "mode"))){
    out <- data.frame(meas = as.numeric(node_measure),
               mode = attr(node_measure, "mode"),
               max1 = max(as.numeric(node_measure)[!attr(node_measure, "mode")]),
               max2 = max(as.numeric(node_measure)[attr(node_measure, "mode")]))
    out <- out$meas == ifelse(out$mode, out$max2, out$max1)
  } else {
    out <- as.numeric(node_measure) == max(as.numeric(node_measure))
  }
  names(out) <- attr(node_measure, "names")
  class(out) <- c("node_mark", class(out))
  out
}

#' @describeIn mark_nodes Returns logical of which nodes 
#'   hold the minimum of some measure
#' @examples 
#' node_is_min(node_degree(ison_brandes))
#' @export
node_is_min <- function(node_measure){
  if(!inherits(node_measure, "node_measure"))
    stop("This function expects an object of class `node_measure`")
  if(any(attr(node_measure, "mode"))){
    out <- data.frame(meas = as.numeric(node_measure),
                      mode = attr(node_measure, "mode"),
                      min1 = min(as.numeric(node_measure)[!attr(node_measure, "mode")]),
                      min2 = min(as.numeric(node_measure)[attr(node_measure, "mode")]))
    out <- out$meas == ifelse(out$mode, out$min2, out$min1)
  } else {
    out <- as.numeric(node_measure) == min(as.numeric(node_measure))
  }
  names(out) <- attr(node_measure, "names")
  class(out) <- c("node_mark", class(out))
  out
}