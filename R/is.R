#' Tests of network properties
#' @name is
#' @family manipulation
#' @param object A migraph-consistent class object
#' (matrix, edgelist, igraph, network, tidygraph)
#' @param ... Additional parameters passed onto the underlying `is_` function.
#' @importFrom igraph is.bipartite
#' @return TRUE if object is a two-mode network, otherwise FALSE
#' @examples
#' is_twomode(southern_women)
#' @export
is_twomode <- function(object) {
  object <- as_igraph(object)
  igraph::is.bipartite(object)
}

#' @rdname is
#' @importFrom igraph is.weighted
#' @return TRUE if object is a weighted network, otherwise FALSE
#' @examples
#' is_weighted(southern_women)
#' @export
is_weighted <- function(object) {
  object <- as_igraph(object)
  igraph::is.weighted(object)
}

#' @rdname is
#' @importFrom igraph is.directed
#' @return TRUE if object is a directed network, otherwise FALSE
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

#' @rdname is
#' @importFrom igraph is.named
#' @return TRUE if object is a labelled network, otherwise FALSE
#' @examples
#' is_labelled(southern_women)
#' @export
is_labelled <- function(object) {
  object <- as_igraph(object)
  igraph::is.named(object)
}

#' @rdname is
#' @importFrom igraph edge_attr_names
#' @return TRUE if object is a signed network, otherwise FALSE
#' @examples
#' is_signed(southern_women)
#' @export
is_signed <- function(object) {
  object <- as_igraph(object)
  "sign" %in% igraph::edge_attr_names(object)
}

#' @rdname is
#' @param method Whether to identify components if only "weak"ly connected
#' or also "strong"ly connected.
#' @importFrom igraph is.connected
#' @return TRUE if object is a connected network, otherwise FALSE
#' @examples
#' is_connected(southern_women)
#' @export
is_connected <- function(object, method = c("weak", "strong")) {
  method <- match.arg(method)
  object <- as_igraph(object)
  igraph::is.connected(object, mode = method)
}

#' @rdname is
#' @importFrom igraph is.loop
#' @export
is_complex <- function(object) {
  object <- as_igraph(object)
  any(igraph::which_loop(object))
}

#' @rdname is
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

#' @rdname is
#' @importFrom igraph is.igraph
#' @importFrom tidygraph is.tbl_graph
#' @importFrom network is.network
#' @export
is_graph <- function(object){
  tidygraph::is.tbl_graph(object) |
    network::is.network(object) |
    igraph::is.igraph(object)
}

#' @rdname is
#' @importFrom igraph is_chordal
#' @export
is_chordal <- function(object, ...){
  object <- as_igraph(object)
  igraph::is_chordal(object, ...)
}

#' @rdname is
#' @importFrom igraph is_dag
#' @export
is_dag <- function(object){
  object <- as_igraph(object)
  igraph::is_dag(object)
}

#' @rdname is
#' @importFrom igraph is_degseq
#' @export
is_degseq <- function(object, ...){
  object <- as_igraph(object)
  if (igraph::is_directed(object)) {
    indeg <- igraph::degree(object, mode = "in")
    outdeg <- igraph::degree(object, mode = "out")
    igraph::is_degseq(indeg, outdeg)
  } else {
    deg <- igraph::degree(object)
    igraph::is_degseq(deg) 
  }
}

#' @rdname is
#' @importFrom igraph is_graphical
#' @export
is_graphical <- function(object, ...){
  object <- as_igraph(object)
  if (igraph::is_directed(object)) {
    indeg <- igraph::degree(object, mode = "in")
    outdeg <- igraph::degree(object, mode = "out")
    igraph::is_graphical(indeg, outdeg)
  } else {
    deg <- igraph::degree(object)
    igraph::is_graphical(deg) 
  }
}

#' @rdname is
#' @importFrom igraph is_matching
#' @export
is_matching <- function(object, matching, types = NULL){
  object <- as_igraph(object)
  igraph::is_matching(object, matching, types)
}

#' @rdname is
#' @importFrom igraph is_max_matching
#' @export
is_max_matching <- function(object, matching, types = NULL){
  object <- as_igraph(object)
  igraph::is_max_matching(object, matching, types)
}

#' @rdname is
#' @importFrom igraph any_multiple
#' @export
any_multiple <- function(object){
  object <- as_igraph(object)
  igraph::any_multiple(object)
}

#' @rdname is
#' @importFrom igraph which_multiple
#' @export
which_multiple <- function(object){
  object <- as_igraph(object)
  igraph::which_multiple(object)
}

#' @rdname is
#' @importFrom igraph which_loop
#' @export
which_multiple <- function(object){
  object <- as_igraph(object)
  igraph::which_loop(object)
}

#' @rdname is
#' @importFrom igraph which_mutual
#' @export
which_mutual <- function(object){
  object <- as_igraph(object) # allow for custom edge selection
  igraph::which_mutual(object)
}

#' @rdname is
#' @importFrom igraph is.simple
#' @export
is_simple <- function(object){
  object <- as_igraph(object)
  igraph::is.simple(object)
}

# Development notes:
# 
# igraph::is_chordal() Done, Needs testing
# igraph::is_dag() Done, Needs testing
# igraph::is.degree.sequence() -> is_degseq() Done, Needs testing
# igraph::is.graphical.degree.sequence() -> is_graphical() Done, Needs testing
# igraph::is.hierarchical()
# igraph::is.loop() -> is_complex Done, needs testing
# igraph::is_matching() Done, needs testing
# igraph::is_max_matching() Done, needs testing
# igraph::max_bipartite_match() Not an is function,
#                               maybe worth implementing somewhere else
# igraph::is.multiple() -> which_multiple() Done, needs testing
# igraph::any_multiple() Done, needs testing
# igraph::which_loop() Done, needs testing
# igraph::is.mutual() 
# igraph::is.simple() Done, needs testing. Might be redundant with the is_complex() function
# 
# We should separate vertex level functions and graph level ones by using which
# and is respectively.