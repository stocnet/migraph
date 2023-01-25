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
  .Deprecated("add_tie_attribute", package = "manynet",
              old = "add_edge_attribute")
  add_tie_attribute(object, attr_name, vector)
}

#' @describeIn defunct Deprecated on 2022-06-28.
#' @export
edge_attribute <- function(object, attribute){
  .Deprecated("tie_attribute", package = "manynet",
              old = "edge_attribute")
  tie_attribute(object, attribute)
}

#' @describeIn defunct Deprecated on 2022-06-28.
#' @export
edge_signs <- function(object){
  .Deprecated("tie_signs", package = "manynet",
              old = "edge_signs")
  tie_signs(object)
}

#' @describeIn defunct Deprecated on 2022-06-28.
#' @export
edge_weights <- function(object){
  .Deprecated("tie_weights", package = "manynet",
              old = "edge_weights")
  tie_weights(object)
}

#' @describeIn defunct Deprecated on 2022-06-28.
#' @export
edge_betweenness <- function(object, normalized = TRUE){
  .Deprecated("tie_betweenness", package = "manynet",
              old = "edge_betweenness")
  tie_betweenness(object, normalized)
}

#' @describeIn defunct Deprecated on 2022-06-28.
#' @export
edge_closeness <- function(object, normalized = TRUE){
  .Deprecated("tie_closeness", package = "manynet",
              old = "edge_closeness")
  tie_closeness(object, normalized)
}

#' @describeIn defunct Deprecated on 2022-06-28.
#' @export
edge_degree <- function(object, normalized = TRUE){
  .Deprecated("tie_degree", package = "manynet",
              old = "edge_degree")
  tie_degree(object, normalized)
}

#' @describeIn defunct Deprecated on 2022-06-28.
#' @export
edge_eigenvector <- function(object, normalized = TRUE){
  .Deprecated("tie_eigenvector", package = "manynet",
              old = "edge_eigenvector")
  tie_eigenvector(object, normalized)
}

#' @describeIn defunct Deprecated on 2022-06-28.
#' @export
edge_loop <- function(object){
  .Deprecated("tie_is_loop", package = "manynet",
              old = "edge_loop")
  tie_is_loop(object)
}

#' @describeIn defunct Deprecated on 2022-06-28.
#' @export
edge_multiple <- function(object){
  .Deprecated("tie_is_multiple", package = "manynet",
              old = "edge_multiple")
  tie_is_multiple(object)
}

#' @describeIn defunct Deprecated on 2022-06-28.
#' @export
edge_bridges <- function(object){
  .Deprecated("tie_is_bridge", package = "manynet",
              old = "edge_bridges")
  tie_is_bridge(object)
}

#' @describeIn defunct Deprecated on 2022-06-28.
#' @export
edge_reciprocal <- function(object){
  .Deprecated("tie_is_reciprocated", package = "manynet",
              old = "edge_reciprocal")
  tie_is_reciprocated(object)
}

#' @describeIn defunct Deprecated on 2022-06-28.
#' @export
graph_edge_attributes <- function(object){
  .Deprecated("network_tie_attributes", package = "manynet",
              old = "graph_edge_attributes")
  network_tie_attributes(object)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_tie_attributes <- function(object){
  .Deprecated("network_tie_attributes", package = "manynet",
              old = "graph_tie_attributes")
  network_tie_attributes(object)
}

#' @describeIn defunct Deprecated on 2022-06-28.
#' @export
graph_edges <- function(object){
  .Deprecated("network_ties", package = "manynet",
              old = "graph_edges")
  network_ties(object)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_ties <- function(object){
  .Deprecated("network_ties", package = "manynet",
              old = "graph_ties")
  network_ties(object)
}

#' @describeIn defunct Deprecated on 2022-06-30.
#' @export
node_cuts <- function(object){
  .Deprecated("node_is_cutpoint", package = "manynet",
              old = "node_cuts")
  node_is_cutpoint(object)
}

#' @describeIn defunct Deprecated on 2022-06-28.
#' @export
to_edges <- function(object){
  .Deprecated("to_ties", package = "manynet",
              old = "to_edges")
  to_ties(object)
}

#' @describeIn defunct Deprecated on 2022-07-03.
#' @export
join_edges <- function(object, object2, attr_name){
  .Deprecated("join_ties", package = "manynet",
              old = "join_edges")
  join_ties(object, object2, attr_name)
}

#' @describeIn defunct Deprecated on 2022-09-07.
#' @export
to_main_component <- function(object) {
  .Deprecated("to_giant", package = "manynet",
              old = "to_main_component")
  to_giant(object)
}

#' @describeIn defunct Deprecated on 2022-09-10.
#' @export
graph_blau_index <- function(object, attribute, clusters = NULL) {
  .Deprecated("network_diversity", package = "manynet",
              old = "graph_blau_index")
  network_diversity(object, attribute = attribute, clusters = clusters)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_diversity <- function(object, attribute, clusters = NULL) {
  .Deprecated("network_diversity", package = "manynet",
              old = "graph_diversity")
  network_diversity(object, attribute = attribute, clusters = clusters)
}

#' @describeIn defunct Deprecated on 2022-09-10.
#' @export
graph_ei_index <- function(object, attribute) {
  .Deprecated("network_homophily", package = "manynet",
              old = "graph_ei_index")
  network_homophily(object, attribute = attribute)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_homophily <- function(object, attribute) {
  .Deprecated("network_homophily", package = "manynet",
              old = "graph_homophily")
  network_homophily(object, attribute = attribute)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
plot.graph_test <- function(x, ...,
                            threshold = .95, 
                            tails = c("two", "one")) {
  .Deprecated("plot.network_test", package = "manynet",
              old = "plot.graph_test")
  plot.network_test(x, ..., 
                    threshold = threshold, 
                    tails = tails)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
print.graph_test <- function(x, ...,
                             max.length = 6,
                             digits = 3) {
  .Deprecated("print.network_test", package = "manynet",
              old = "print.graph_test")
  print.network_test(x, ..., 
                     max.length = max.length,
                     digits = digits)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
print.graph_measure <- function(x, ...,
                             digits = 3) {
  .Deprecated("print.network_measure", package = "manynet",
              old = "print.graph_measure")
  print.network_measure(x, ...,
                     digits = digits)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
print.graph_motif <- function(x, ...) {
  .Deprecated("print.network_motif", package = "manynet",
              old = "print.graph_motif")
  print.network_motif(x, ...)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_adhesion <- function(object) {
  .Deprecated("network_adhesion", package = "manynet",
              old = "graph_adhesion")
  network_adhesion(object)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_cohesion <- function(object) {
  .Deprecated("network_cohesion", package = "manynet",
              old = "graph_cohesion")
  network_cohesion(object)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_assortativity <- function(object) {
  .Deprecated("network_assortativity", package = "manynet",
              old = "graph_assortativity")
  network_assortativity(object)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_balance <- function(object) {
  .Deprecated("network_balance", package = "manynet",
              old = "graph_balance")
  network_balance(object)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_betweenness <- function(object, normalized = TRUE,
                              direction = c("all", "out", "in")) {
  .Deprecated("network_betweenness", package = "manynet",
              old = "graph_betweenness")
  network_betweenness(object, normalized = normalized,
                      direction = direction)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_closeness <- function(object, normalized = TRUE,
                              direction = c("all", "out", "in")) {
  .Deprecated("network_closeness", package = "manynet",
              old = "graph_closeness")
  network_closeness(object, normalized = normalized,
                      direction = direction)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_degree <- function(object, normalized = TRUE,
                              direction = c("all", "out", "in")) {
  .Deprecated("network_degree", package = "manynet",
              old = "graph_degree")
  network_degree(object, normalized = normalized,
                      direction = direction)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_eigenvector <- function(object, normalized = TRUE) {
  .Deprecated("network_eigenvector", package = "manynet",
              old = "graph_eigenvector")
  network_eigenvector(object, normalized = normalized)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_components <- function(object) {
  .Deprecated("network_components", package = "manynet",
              old = "graph_components")
  network_components(object)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_factions <- function(object, membership = NULL) {
  .Deprecated("network_factions", package = "manynet",
              old = "graph_factions")
  network_factions(object, membership = membership)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_congruency <- function(object, object2) {
  .Deprecated("network_congruency", package = "manynet",
              old = "graph_congruency")
  network_congruency(object, object2)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_equivalency <- function(object) {
  .Deprecated("network_equivalency", package = "manynet",
              old = "graph_equivalency")
  network_equivalency(object)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_core <- function(object, membership = NULL) {
  .Deprecated("network_core", package = "manynet",
              old = "graph_core")
  network_core(object, membership = membership)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_density <- function(object) {
  .Deprecated("network_density", package = "manynet",
              old = "graph_density")
  network_density(object)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_reciprocity <- function(object, method = "default") {
  .Deprecated("network_reciprocity", package = "manynet",
              old = "graph_reciprocity")
  network_reciprocity(object, method = method)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_transitivity <- function(object) {
  .Deprecated("network_transitivity", package = "manynet",
              old = "graph_transitivity")
  network_transitivity(object)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_diameter <- function(object) {
  .Deprecated("network_diameter", package = "manynet",
              old = "graph_diameter")
  network_diameter(object)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_length <- function(object) {
  .Deprecated("network_length", package = "manynet",
              old = "graph_length")
  network_length(object)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_dims <- function(object) {
  .Deprecated("network_dims", package = "manynet",
              old = "graph_dims")
  network_dims(object)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_nodes <- function(object) {
  .Deprecated("network_nodes", package = "manynet",
              old = "graph_nodes")
  network_nodes(object)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_dyad_census <- function(object) {
  .Deprecated("network_dyad_census", package = "manynet",
              old = "graph_dyad_census")
  network_dyad_census(object)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_triad_census <- function(object) {
  .Deprecated("network_triad_census", package = "manynet",
              old = "graph_triad_census")
  network_triad_census(object)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_mixed_census <- function(object, object2) {
  .Deprecated("network_mixed_census", package = "manynet",
              old = "graph_mixed_census")
  network_mixed_census(object, object2)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_modularity <- function(object, membership = NULL, resolution = 1) {
  .Deprecated("network_modularity", package = "manynet",
              old = "graph_modularity")
  network_modularity(object, membership = membership,
                     resolution = resolution)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_node_attributes <- function(object) {
  .Deprecated("network_node_attributes", package = "manynet",
              old = "graph_node_attributes")
  network_node_attributes(object)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_smallworld <- function(object, times = 100) {
  .Deprecated("network_smallworld", package = "manynet",
              old = "graph_smallworld")
  network_smallworld(object, times = times)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
network_homophily <- function(object, attribute) {
  .Deprecated("network_heterophily", package = "manynet",
              old = "network_homophily")
  network_heterophily(object, attribute)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
node_homophily <- function(object, attribute) {
  .Deprecated("node_heterophily", package = "manynet",
              old = "node_homophily")
  node_heterophily(object, attribute)
}

