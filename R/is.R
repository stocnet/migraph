#' Tests of network properties
#' @name is
#' @param object A migraph-consistent class object (matrix, edgelist, igraph, network, tidygraph)
#' @importFrom igraph is.bipartite
#' @return TRUE if object is a two-mode network, otherwise FALSE
#' @examples
#' is_twomode(southern_women)
#' @export
is_twomode <- function(object){
  object <- as_igraph(object)
  igraph::is.bipartite(object)
}

#' @rdname is
#' @importFrom igraph is.weighted
#' @return TRUE if object is a weighted network, otherwise FALSE
#' @examples
#' is_weighted(southern_women)
#' @export
is_weighted <- function(object){
  object <- as_igraph(object)
  igraph::is.weighted(object)
}

#' @rdname is
#' @importFrom igraph is.directed
#' @return TRUE if object is a directed network, otherwise FALSE
#' @examples
#' is_directed(southern_women)
#' @export
is_directed <- function(object){
  object <- as_igraph(object)
  igraph::is.directed(object)
}

#' @rdname is
#' @importFrom igraph is.named
#' @return TRUE if object is a labelled network, otherwise FALSE
#' @examples
#' is_labelled(southern_women)
#' @export
is_labelled <- function(object){
  object <- as_igraph(object)
  igraph::is.named(object)
}

# igraph::is.chordal()
# igraph::is.connected()
# igraph::is.dag()
# igraph::is.degree.sequence()
# igraph::is.graphical.degree.sequence()
# igraph::is.hierarchical()
# igraph::is.loop()
# igraph::is.matching()
# igraph::is.multiple()
# igraph::is.mutual()
# igraph::is.simple()