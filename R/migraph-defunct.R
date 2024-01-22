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
  manynet::tie_is_loop(object)
}

#' @describeIn defunct Deprecated on 2022-06-28.
#' @export
edge_multiple <- function(object){
  .Deprecated("tie_is_multiple", package = "migraph",
              old = "edge_multiple")
  manynet::tie_is_multiple(object)
}

#' @describeIn defunct Deprecated on 2022-06-28.
#' @export
edge_bridges <- function(object){
  .Deprecated("tie_is_bridge", package = "migraph",
              old = "edge_bridges")
  manynet::tie_is_bridge(object)
}

#' @describeIn defunct Deprecated on 2022-06-28.
#' @export
edge_reciprocal <- function(object){
  .Deprecated("tie_is_reciprocated", package = "migraph",
              old = "edge_reciprocal")
  manynet::tie_is_reciprocated(object)
}

#' @describeIn defunct Deprecated on 2022-06-30.
#' @export
node_cuts <- function(object){
  .Deprecated("node_is_cutpoint", package = "migraph",
              old = "node_cuts")
  manynet::node_is_cutpoint(object)
}

#' @describeIn defunct Deprecated on 2022-09-10.
#' @export
graph_blau_index <- function(object, attribute, clusters = NULL) {
  .Deprecated("network_diversity", package = "migraph",
              old = "graph_blau_index")
  network_diversity(object, attribute = attribute, clusters = clusters)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_diversity <- function(object, attribute, clusters = NULL) {
  .Deprecated("network_diversity", package = "migraph",
              old = "graph_diversity")
  network_diversity(object, attribute = attribute, clusters = clusters)
}

#' @describeIn defunct Deprecated on 2022-09-10.
#' @export
graph_ei_index <- function(object, attribute) {
  .Deprecated("network_homophily", package = "migraph",
              old = "graph_ei_index")
  network_homophily(object, attribute = attribute)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_homophily <- function(object, attribute) {
  .Deprecated("network_homophily", package = "migraph",
              old = "graph_homophily")
  network_homophily(object, attribute = attribute)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
plot.graph_test <- function(x, ...,
                            threshold = .95, 
                            tails = c("two", "one")) {
  .Deprecated("plot.network_test", package = "migraph",
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
  .Deprecated("print.network_test", package = "migraph",
              old = "print.graph_test")
  print.network_test(x, ..., 
                     max.length = max.length,
                     digits = digits)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
print.graph_measure <- function(x, ...,
                             digits = 3) {
  .Deprecated("print.network_measure", package = "migraph",
              old = "print.graph_measure")
  print.network_measure(x, ...,
                     digits = digits)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
print.graph_motif <- function(x, ...) {
  .Deprecated("print.network_motif", package = "migraph",
              old = "print.graph_motif")
  print.network_motif(x, ...)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_adhesion <- function(object) {
  .Deprecated("network_adhesion", package = "migraph",
              old = "graph_adhesion")
  network_adhesion(object)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_cohesion <- function(object) {
  .Deprecated("network_cohesion", package = "migraph",
              old = "graph_cohesion")
  network_cohesion(object)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_assortativity <- function(object) {
  .Deprecated("network_assortativity", package = "migraph",
              old = "graph_assortativity")
  network_assortativity(object)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_balance <- function(object) {
  .Deprecated("network_balance", package = "migraph",
              old = "graph_balance")
  network_balance(object)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_betweenness <- function(object, normalized = TRUE,
                              direction = c("all", "out", "in")) {
  .Deprecated("network_betweenness", package = "migraph",
              old = "graph_betweenness")
  network_betweenness(object, normalized = normalized,
                      direction = direction)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_closeness <- function(object, normalized = TRUE,
                              direction = c("all", "out", "in")) {
  .Deprecated("network_closeness", package = "migraph",
              old = "graph_closeness")
  network_closeness(object, normalized = normalized,
                      direction = direction)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_degree <- function(object, normalized = TRUE,
                              direction = c("all", "out", "in")) {
  .Deprecated("network_degree", package = "migraph",
              old = "graph_degree")
  network_degree(object, normalized = normalized,
                      direction = direction)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_eigenvector <- function(object, normalized = TRUE) {
  .Deprecated("network_eigenvector", package = "migraph",
              old = "graph_eigenvector")
  network_eigenvector(object, normalized = normalized)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_components <- function(object) {
  .Deprecated("network_components", package = "migraph",
              old = "graph_components")
  network_components(object)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_factions <- function(object, membership = NULL) {
  .Deprecated("network_factions", package = "migraph",
              old = "graph_factions")
  network_factions(object, membership = membership)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_congruency <- function(object, object2) {
  .Deprecated("network_congruency", package = "migraph",
              old = "graph_congruency")
  network_congruency(object, object2)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_equivalency <- function(object) {
  .Deprecated("network_equivalency", package = "migraph",
              old = "graph_equivalency")
  network_equivalency(object)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_core <- function(object, membership = NULL) {
  .Deprecated("network_core", package = "migraph",
              old = "graph_core")
  network_core(object, membership = membership)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_density <- function(object) {
  .Deprecated("network_density", package = "migraph",
              old = "graph_density")
  network_density(object)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_reciprocity <- function(object, method = "default") {
  .Deprecated("network_reciprocity", package = "migraph",
              old = "graph_reciprocity")
  network_reciprocity(object, method = method)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_transitivity <- function(object) {
  .Deprecated("network_transitivity", package = "migraph",
              old = "graph_transitivity")
  network_transitivity(object)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_diameter <- function(object) {
  .Deprecated("network_diameter", package = "migraph",
              old = "graph_diameter")
  network_diameter(object)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_length <- function(object) {
  .Deprecated("network_length", package = "migraph",
              old = "graph_length")
  network_length(object)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_dyad_census <- function(object) {
  .Deprecated("network_dyad_census", package = "migraph",
              old = "graph_dyad_census")
  network_dyad_census(object)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_triad_census <- function(object) {
  .Deprecated("network_triad_census", package = "migraph",
              old = "graph_triad_census")
  network_triad_census(object)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_mixed_census <- function(object, object2) {
  .Deprecated("network_mixed_census", package = "migraph",
              old = "graph_mixed_census")
  network_mixed_census(object, object2)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_modularity <- function(object, membership = NULL, resolution = 1) {
  .Deprecated("network_modularity", package = "migraph",
              old = "graph_modularity")
  network_modularity(object, membership = membership,
                     resolution = resolution)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
graph_smallworld <- function(object, times = 100) {
  .Deprecated("network_smallworld", package = "migraph",
              old = "graph_smallworld")
  network_smallworld(object, times = times)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
network_homophily <- function(object, attribute) {
  .Deprecated("network_heterophily", package = "migraph",
              old = "network_homophily")
  network_heterophily(object, attribute)
}

#' @describeIn defunct Deprecated on 2022-09-25.
#' @export
node_homophily <- function(object, attribute) {
  .Deprecated("node_heterophily", package = "migraph",
              old = "node_homophily")
  node_heterophily(object, attribute)
}
