#' Tests of network properties
#' 
#' These functions implement tests for various network
#' properties.
#' @param object A migraph-consistent class object
#' (matrix, edgelist, igraph, network, tidygraph)
#' @return TRUE if the condition is met, or FALSE otherwise.
#' @family manipulation
#' @name is
NULL

#' @describeIn is Tests whether network is migraph-compatible
#' @importFrom igraph is.igraph
#' @importFrom tidygraph is.tbl_graph
#' @importFrom network is.network
#' @export
is_migraph <- function(object){
  tidygraph::is.tbl_graph(object) |
    network::is.network(object) |
    igraph::is.igraph(object) |
    is.data.frame(object) |
    is.matrix(object)
}

#' @describeIn is Tests whether network contains graph-level information
#' @importFrom igraph is.igraph
#' @importFrom tidygraph is.tbl_graph
#' @importFrom network is.network
#' @export
is_graph <- function(object){
  tidygraph::is.tbl_graph(object) |
    network::is.network(object) |
    igraph::is.igraph(object)
}

#' @describeIn is Tests whether network is a two-mode network
#' @importFrom igraph is.bipartite
#' @examples
#' is_twomode(southern_women)
#' @export
is_twomode <- function(object) {
  object <- as_igraph(object)
  igraph::is.bipartite(object)
}

#' @describeIn is Tests whether network is weighted
#' @importFrom igraph is.weighted
#' @examples
#' is_weighted(southern_women)
#' @export
is_weighted <- function(object) {
  object <- as_igraph(object)
  igraph::is.weighted(object)
}

#' @describeIn is Tests whether network is directed
#' @importFrom igraph is.directed
#' @examples
#' is_directed(southern_women)
#' @export
is_directed <- function(object) {
  if(network::is.network(object)){
    object$gal$directed
  } else {
    object <- as_igraph(object)
    igraph::is.directed(object)
  }
}

#' @describeIn is Tests whether network includes names for the nodes
#' @importFrom igraph is.named
#' @examples
#' is_labelled(southern_women)
#' @export
is_labelled <- function(object) {
  object <- as_igraph(object)
  igraph::is.named(object)
}

#' @describeIn is Tests whether network is signed positive/negative
#' @importFrom igraph edge_attr_names
#' @examples
#' is_signed(southern_women)
#' @export
is_signed <- function(object) {
  object <- as_igraph(object)
  "sign" %in% igraph::edge_attr_names(object)
}

#' @describeIn is Tests whether network is (weakly/strongly) connected
#' @param method Whether to identify components if only "weak"ly connected
#' or also "strong"ly connected.
#' @importFrom igraph is.connected
#' @examples
#' is_connected(southern_women)
#' @export
is_connected <- function(object, method = c("weak", "strong")) {
  method <- match.arg(method)
  object <- as_igraph(object)
  igraph::is.connected(object, mode = method)
}

#' @describeIn is Tests whether network contains any loops
#' @importFrom igraph is.loop
#' @examples
#' is_complex(southern_women)
#' @export
is_complex <- function(object) {
  object <- as_igraph(object)
  any(igraph::which_loop(object))
}

#' @describeIn is Tests whether network is multiplex
#' @importFrom igraph any_multiple
#' @export
is_multiplex <- function(object){
  object <- as_igraph(object)
  igraph::any_multiple(object)
}

#' @describeIn is Tests whether network is simple (both uniplex and simplex)
#' @importFrom igraph is.simple
#' @examples 
#' is_uniplex(ison_m182)
#' @export
is_uniplex <- function(object){
  object <- as_igraph(object)
  igraph::is.simple(object)
}

#' @describeIn is Tests whether network is a directed acyclic graph
#' @importFrom igraph is_dag
#' @examples 
#' is_acyclic(ison_m182)
#' @export
is_acyclic <- function(object){
  object <- as_igraph(object)
  igraph::is_dag(object)
}
