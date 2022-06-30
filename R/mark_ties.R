#' Logical whether nodes or ties meet some criteria
#' 
#' These functions return logical vectors of the length of the 
#' nodes or ties in a network identifying which meet some criteria.
#' Such functions are often useful for identifying nodes or ties 
#' that are particularly well- or poorly-connected.
#' @inheritParams is
#' @family marks
#' @name mark
NULL

#' @describeIn mark Returns logical of which ties are multiples
#' @importFrom igraph which_multiple
#' @examples 
#' tie_is_multiple(ison_marvel_relationships)
#' @export
tie_is_multiple <- function(object){
  object <- as_igraph(object)
  make_tie_mark(igraph::which_multiple(object), object)
}

#' @describeIn mark Returns logical of which ties are loops
#' @importFrom igraph which_loop
#' @examples 
#' tie_is_loop(ison_marvel_relationships)
#' @export
tie_is_loop <- function(object){
  object <- as_igraph(object)
  make_tie_mark(igraph::which_loop(object), object)
}

#' @describeIn mark Returns logical of which ties 
#'   are mutual/reciprocated
#' @importFrom igraph which_mutual
#' @examples 
#' tie_is_reciprocal(ison_algebra)
#' @export
tie_is_reciprocal <- function(object){
  object <- as_igraph(object) # allow for custom edge selection
  make_tie_mark(igraph::which_mutual(object), object)
}

#' @describeIn mark Returns logical of which ties cut
#'   or act as articulation points in a network.
#' @importFrom igraph decompose delete.edges
#' @examples 
#' tie_is_bridge(ison_brandes)
#' @export
tie_is_bridge <- function(object){
  num_comp <- length( igraph::decompose(as_igraph(object)) )
  out <- vapply(seq_len(graph_ties(object)), function(x){
    length( igraph::decompose(igraph::delete.edges(object, x)) ) > num_comp
  }, FUN.VALUE = logical(1))
  if(is_labelled(object)) 
    names(out) <- attr(igraph::E(object), "vnames")
  make_tie_mark(out, object)
}
