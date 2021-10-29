#' Tests of network properties
#' @name is
#' @family manipulation
#' @param object A migraph-consistent class object
#' (matrix, edgelist, igraph, network, tidygraph)
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
  object <- as_igraph(object)
  igraph::is.directed(object)
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

# igraph::is.chordal()
# igraph::is.dag()
# igraph::is.degree.sequence()
# igraph::is.graphical.degree.sequence()
# igraph::is.hierarchical()
# igraph::is.loop()
# igraph::is.matching()
# igraph::is.multiple()
# igraph::is.mutual()
# igraph::is.simple()