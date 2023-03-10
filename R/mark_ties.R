#' Marking ties based on their properties
#' 
#' @description 
#'   These functions return logical vectors the length of the ties
#'   in a network, identifying which hold some property.
#'   They are most useful in highlighting parts of the network that
#'   are particularly well- or poorly-connected.
#' @inheritParams is
#' @family marks
#' @name mark_ties
NULL

#' @describeIn mark_ties Returns logical of which ties are multiples
#' @importFrom igraph which_multiple
#' @examples 
#' tie_is_multiple(ison_marvel_relationships)
#' @export
tie_is_multiple <- function(.data){
  object <- as_igraph(.data)
  make_tie_mark(igraph::which_multiple(.data), .data)
}

#' @describeIn mark_ties Returns logical of which ties are loops
#' @importFrom igraph which_loop
#' @examples 
#' tie_is_loop(ison_marvel_relationships)
#' @export
tie_is_loop <- function(.data){
  .data <- as_igraph(.data)
  make_tie_mark(igraph::which_loop(.data), .data)
}

#' @describeIn mark_ties Returns logical of which ties 
#'   are mutual/reciprocated
#' @importFrom igraph which_mutual
#' @examples 
#' tie_is_reciprocated(ison_algebra)
#' @export
tie_is_reciprocated <- function(.data){
  .data <- as_igraph(.data) # allow for custom edge selection
  make_tie_mark(igraph::which_mutual(.data), .data)
}

#' @describeIn mark_ties Returns logical of which ties cut
#'   or act as articulation points in a network.
#' @importFrom igraph decompose delete.edges
#' @examples 
#' tie_is_bridge(ison_brandes)
#' @export
tie_is_bridge <- function(.data){
  num_comp <- length( igraph::decompose(as_igraph(.data)) )
  out <- vapply(seq_len(network_ties(.data)), function(x){
    length( igraph::decompose(igraph::delete.edges(.data, x)) ) > num_comp
  }, FUN.VALUE = logical(1))
  if(is_labelled(.data)) 
    names(out) <- attr(igraph::E(.data), "vnames")
  make_tie_mark(out, .data)
}

#' @describeIn mark_ties Returns logical of which ties 
#'   hold the maximum of some measure
#' @param tie_measure An object created by a `tie_` measure.
#' @examples 
#' tie_is_max(tie_betweenness(ison_brandes))
#' @export
tie_is_max <- function(tie_measure){
  out <- as.numeric(tie_measure) == max(as.numeric(tie_measure))
  class(out) <- c("tie_mark", class(out))
  out
}

#' @describeIn mark_ties Returns logical of which ties 
#'   hold the minimum of some measure
#' @examples 
#' tie_is_min(tie_betweenness(ison_brandes))
#' @export
tie_is_min <- function(tie_measure){
  out <- as.numeric(tie_measure) == min(as.numeric(tie_measure))
  class(out) <- c("tie_mark", class(out))
  out
}
