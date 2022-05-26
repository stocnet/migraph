#' Component partitioning algorithms
#' @inheritParams is
#' @name components
NULL

#' @describeIn components Returns nodes' component membership
#'   ignoring edge direction.
#' @importFrom igraph components
#' @examples 
#' node_weak_components(mpn_bristol)
#' @export
node_weak_components <- function(object){
  if(!is_graph(object)) object <- as_igraph(object)
  make_partition(igraph::components(object, mode = "weak")$membership,
                 object)
}

#' @describeIn components Returns nodes' component membership
#'   based on edge direction.
#' @importFrom igraph components
#' @examples 
#' node_strong_components(mpn_bristol)
#' @export
node_strong_components <- function(object){
  if(!is_graph(object)) object <- as_igraph(object)
  make_partition(igraph::components(object, mode = "strong")$membership,
                 object)
}

#' @describeIn components Returns k-cores
#' @examples
#' node_coreness(ison_adolescents)
#' @export
node_coreness <- function(object){
  if(!is_graph(object)) object <- as_igraph(object)
  out <- igraph::coreness(object)
  make_partition(out, object)
}

