#' Marking networks based on their properties
#' 
#' These functions implement logical tests for various network
#' properties.
#' @param object An object of a migraph-consistent class:
#'   \itemize{
#'   \item matrix (adjacency or incidence) from `{base}` R
#'   \item edgelist, a data frame from `{base}` R or tibble from `{tibble}`
#'   \item igraph, from the `{igraph}` package
#'   \item network, from the `{network}` package
#'   \item tbl_graph, from the `{tidygraph}` package
#'   }
#' @return TRUE if the condition is met, or FALSE otherwise.
#' @family marks
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
    (is.data.frame(object) & 
       "from" %in% names(object) & "to" %in% names(object)) |
    (is.matrix(object) & is.numeric(object))
}

#' @describeIn is Tests whether network contains graph-level information
#' @importFrom igraph is.igraph
#' @importFrom tidygraph is.tbl_graph
#' @importFrom network is.network
#' @export
is_graph <- function(object) UseMethod("is_graph")

#' @export
is_graph.data.frame <- function(object){
  FALSE
}

#' @export
is_graph.matrix <- function(object){
  FALSE
}

#' @export
is_graph.tbl_graph <- function(object){
  tidygraph::is.tbl_graph(object)
}

#' @export
is_graph.igraph <- function(object){
  igraph::is.igraph(object)
}

#' @export
is_graph.network <- function(object){
  FALSE
}

#' @describeIn is Tests whether data frame is an edgelist
#' @export
is_edgelist <- function(object) UseMethod("is_edgelist")
  
#' @export
is_edgelist.data.frame <- function(object){
  ncol(object) >= 2 & "from" %in% names(object) & "to" %in% names(object)
}

#' @export
is_edgelist.matrix <- function(object){
  FALSE
}

#' @export
is_edgelist.network <- function(object){
  FALSE
}

#' @export
is_edgelist.igraph <- function(object){
  FALSE
}

#' @export
is_edgelist.tbl_graph <- function(object){
  FALSE
}

#' @describeIn is Tests whether network is a two-mode network
#' @importFrom igraph is.bipartite
#' @examples
#' is_twomode(ison_southern_women)
#' @export
is_twomode <- function(object) UseMethod("is_twomode")

#' @export
is_twomode.igraph <- function(object) {
  igraph::is.bipartite(object)
}

#' @export
is_twomode.tbl_graph <- function(object) {
  igraph::is.bipartite(object)
}

#' @export
is_twomode.matrix <- function(object) {
  dim(object)[1] != dim(object)[2]
}

#' @export
is_twomode.network <- function(object) {
  object <- as_matrix(object)
  dim(object)[1] != dim(object)[2]
}

#' @export
is_twomode.data.frame <- function(object) {
  is_edgelist(object) && 
    length(intersect(object[,1], object[,2])) == 0
}

#' @describeIn is Tests whether network is weighted
#' @importFrom igraph is.weighted
#' @examples
#' is_weighted(ison_southern_women)
#' @export
is_weighted <- function(object) UseMethod("is_weighted")

#' @export
is_weighted.igraph <- function(object) {
  igraph::is.weighted(object)
}

#' @export
is_weighted.tbl_graph <- function(object) {
  igraph::is.weighted(object)
}

#' @export
is_weighted.matrix <- function(object) {
  !all(object == 0 | object == 1)
}

#' @export
is_weighted.network <- function(object) {
  "weight" %in% network::list.edge.attributes(object)
}

#' @export
is_weighted.data.frame <- function(object) {
  ncol(object)>=3 && 
    ("weight" %in% names(object) | is.numeric(object[,3]))
}

#' @describeIn is Tests whether network is directed
#' @importFrom igraph is.directed
#' @examples
#' is_directed(ison_algebra)
#' @export
is_directed <- function(object) UseMethod("is_directed")

#' @export
is_directed.data.frame <- function(object) {
  !(graph_reciprocity(object) == 0 |
    graph_reciprocity(object) == 1)
}

#' @export
is_directed.igraph <- function(object) {
  if(is_twomode(object)) FALSE else igraph::is.directed(object)
}

#' @export
is_directed.tbl_graph <- function(object) {
  if(is_twomode(object)) FALSE else igraph::is.directed(object)
}

#' @export
is_directed.network <- function(object) {
    object$gal$directed
}

#' @export
is_directed.matrix <- function(object) {
  if(is_twomode(object)) FALSE else !isSymmetric(object)
}

#' @describeIn is Tests whether network includes names for the nodes
#' @importFrom igraph is.named
#' @examples
#' is_labelled(ison_southern_women)
#' @export
is_labelled <- function(object) UseMethod("is_labelled")

#' @export
is_labelled.igraph <- function(object) {
  igraph::is.named(object)
}

#' @export
is_labelled.tbl_graph <- function(object) {
  igraph::is.named(object)
}

#' @export
is_labelled.matrix <- function(object) {
  !is.null(dimnames(object))
}

#' @export
is_labelled.network <- function(object) {
  object <- as_matrix(object)
  !is.null(dimnames(object))
}

#' @export
is_labelled.data.frame <- function(object) {
  is.character(object[,1]) & is.character(object[,2])
}

#' @describeIn is Tests whether network is signed positive/negative
#' @importFrom igraph edge_attr_names
#' @examples
#' is_signed(ison_southern_women)
#' @export
is_signed <- function(object) UseMethod("is_signed")

is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  
  abs(x - round(x)) < tol

#' @export
is_signed.data.frame <- function(object) {
  all(is.wholenumber(object[,3])) && any(object[,3] < 0)
}

#' @export
is_signed.matrix <- function(object) {
  all(is.wholenumber(c(object))) && any(object < 0)
}

#' @export
is_signed.igraph <- function(object) {
  "sign" %in% igraph::edge_attr_names(object)
}

#' @export
is_signed.tbl_graph <- function(object) {
  "sign" %in% igraph::edge_attr_names(object)
}

#' @export
is_signed.network <- function(object) {
  "sign" %in% network::list.edge.attributes(object)
}

#' @describeIn is Tests whether network is weakly connected if
#'   the network is undirected or strongly connected if directed.
#'   To test weak connection on a directed network,
#'   please see `to_undirected()`.
#' @importFrom igraph is.connected
#' @examples
#' is_connected(ison_southern_women)
#' @export
is_connected <- function(object) {
  igraph::is.connected(as_igraph(object), 
                       mode = ifelse(is_directed(object),
                                     "strong", "weak"))
}

#' @describeIn is Tests whether network contains any loops
#' @importFrom igraph is.loop
#' @examples
#' is_complex(ison_southern_women)
#' @export
is_complex <- function(object) UseMethod("is_complex")

#' @export
is_complex.igraph <- function(object) {
  any(igraph::which_loop(object))
}

#' @export
is_complex.tbl_graph <- function(object) {
  any(igraph::which_loop(object))
}

#' @export
is_complex.matrix <- function(object) {
  !(is_twomode(object) || all(is.na(diag(object))) || all(diag(object) == 0))
}

#' @export
is_complex.data.frame <- function(object) {
  any(object[,1] == object[,2])
}

#' @export
is_complex.network <- function(object) {
  network::has.loops(object)
}

#' @describeIn is Tests whether network is multiplex,
#'   either from multiple rows with the same sender and receiver,
#'   or multiple columns to the edgelist.
#' @importFrom igraph any_multiple
#' @export
is_multiplex <- function(object) UseMethod("is_multiplex")

#' @export
is_multiplex.matrix <- function(object){
  FALSE
}

#' @export
is_multiplex.tbl_graph <- function(object){
  igraph::any_multiple(object) |
    length(graph_tie_attributes(object)) > 1
}

#' @export
is_multiplex.igraph <- function(object){
  igraph::any_multiple(object) |
    length(graph_tie_attributes(object)) > 1
}

#' @export
is_multiplex.network <- function(object){
  network::is.multiplex(object)
}

#' @export
is_multiplex.data.frame <- function(object){
  ncol(object) > 3
}

#' @describeIn is Tests whether network is simple (both uniplex and simplex)
#' @importFrom igraph is.simple
#' @examples 
#' is_uniplex(ison_algebra)
#' @export
is_uniplex <- function(object){
  object <- as_igraph(object)
  igraph::is.simple(object)
}

#' @describeIn is Tests whether network is a directed acyclic graph
#' @importFrom igraph is_dag
#' @examples 
#' is_acyclic(ison_algebra)
#' @export
is_acyclic <- function(object){
  object <- as_igraph(object)
  igraph::is_dag(object)
}
