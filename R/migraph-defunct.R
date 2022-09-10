#' Functions that have been renamed, superseded, or are no longer working
#' 
#' `r lifecycle::badge("deprecated")`
#' Generally these functions have been superseded or renamed.
#' Upon using them, a message is provided directing the user to the new function.
#' However, at this stage of package development,
#' we generally clear older defunct functions at each minor release,
#' and so you are strongly encouraged to use the new functions/names/syntax
#' wherever possible and update your scripts accordingly.
#' @name defunct
#' @keywords internal
NULL

#' @describeIn defunct Deprecated on 2022-06-28.
#' @export
add_edge_attribute <- function(object, attr_name, vector){
  .Deprecated("add_tie_attribute", package = "migraph",
              old = "add_edge_attribute")
  add_tie_attribute(object, attr_name, vector)
}

#' @describeIn defunct Deprecated on 2022-06-28.
#' @export
edge_attribute <- function(object, attribute){
  .Deprecated("tie_attribute", package = "migraph",
              old = "edge_attribute")
  tie_attribute(object, attribute)
}

#' @describeIn defunct Deprecated on 2022-06-28.
#' @export
edge_signs <- function(object){
  .Deprecated("tie_signs", package = "migraph",
              old = "edge_signs")
  tie_signs(object)
}

#' @describeIn defunct Deprecated on 2022-06-28.
#' @export
edge_weights <- function(object){
  .Deprecated("tie_weights", package = "migraph",
              old = "edge_weights")
  tie_weights(object)
}

#' @describeIn defunct Deprecated on 2022-06-28.
#' @export
edge_betweenness <- function(object, normalized = TRUE){
  .Deprecated("tie_betweenness", package = "migraph",
              old = "edge_betweenness")
  tie_betweenness(object, normalized)
}

#' @describeIn defunct Deprecated on 2022-06-28.
#' @export
edge_closeness <- function(object, normalized = TRUE){
  .Deprecated("tie_closeness", package = "migraph",
              old = "edge_closeness")
  tie_closeness(object, normalized)
}

#' @describeIn defunct Deprecated on 2022-06-28.
#' @export
edge_degree <- function(object, normalized = TRUE){
  .Deprecated("tie_degree", package = "migraph",
              old = "edge_degree")
  tie_degree(object, normalized)
}

#' @describeIn defunct Deprecated on 2022-06-28.
#' @export
edge_eigenvector <- function(object, normalized = TRUE){
  .Deprecated("tie_eigenvector", package = "migraph",
              old = "edge_eigenvector")
  tie_eigenvector(object, normalized)
}

#' @describeIn defunct Deprecated on 2022-06-28.
#' @export
edge_loop <- function(object){
  .Deprecated("tie_is_loop", package = "migraph",
              old = "edge_loop")
  tie_is_loop(object)
}

#' @describeIn defunct Deprecated on 2022-06-28.
#' @export
edge_multiple <- function(object){
  .Deprecated("tie_is_multiple", package = "migraph",
              old = "edge_multiple")
  tie_is_multiple(object)
}

#' @describeIn defunct Deprecated on 2022-06-28.
#' @export
edge_bridges <- function(object){
  .Deprecated("tie_is_bridge", package = "migraph",
              old = "edge_bridges")
  tie_is_bridge(object)
}

#' @describeIn defunct Deprecated on 2022-06-28.
#' @export
edge_reciprocal <- function(object){
  .Deprecated("tie_is_reciprocated", package = "migraph",
              old = "edge_reciprocal")
  tie_is_reciprocated(object)
}

#' @describeIn defunct Deprecated on 2022-06-28.
#' @export
graph_edge_attributes <- function(object){
  .Deprecated("graph_tie_attributes", package = "migraph",
              old = "graph_edge_attributes")
  graph_tie_attributes(object)
}

#' @describeIn defunct Deprecated on 2022-06-28.
#' @export
graph_edges <- function(object){
  .Deprecated("graph_ties", package = "migraph",
              old = "graph_edges")
  graph_ties(object)
}

#' @describeIn defunct Deprecated on 2022-06-30.
#' @export
node_cuts <- function(object){
  .Deprecated("node_is_cutpoint", package = "migraph",
              old = "node_cuts")
  node_is_cutpoint(object)
}

#' @describeIn defunct Deprecated on 2022-06-28.
#' @export
to_edges <- function(object){
  .Deprecated("to_ties", package = "migraph",
              old = "to_edges")
  to_ties(object)
}

#' @describeIn defunct Deprecated on 2022-07-03.
#' @export
join_edges <- function(object, object2, attr_name){
  .Deprecated("join_ties", package = "migraph",
              old = "join_edges")
  join_ties(object, object2, attr_name)
}

#' @describeIn defunct Deprecated on 2022-09-07.
#' @export
to_main_component <- function(object) {
  .Deprecated("to_giant", package = "migraph",
              old = "to_main_component")
  to_giant(object)
}

#' @describeIn defunct Deprecated on 2022-09-10.
#' @export
graph_blau_index <- function(object, attribute, clusters = NULL) {
  .Deprecated("graph_diversity", package = "migraph",
              old = "graph_blau_index")
  graph_diversity(object, attribute, clusters)
}

#' @describeIn defunct Deprecated on 2022-09-10.
#' @export
graph_ei_index <- function(object, attribute) {
  .Deprecated("graph_homophily", package = "migraph",
              old = "graph_ei_index")
  graph_homophily(object, attribute)
}
