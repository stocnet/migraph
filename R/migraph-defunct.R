#' Functions that have been renamed, superseded, or are no longer working
#' 
#' `r lifecycle::badge("deprecated")`
#' @name defunct
#' @keywords internal
NULL

#' @describeIn defunct Deprecated on 2021-10-18.
#' Returns `graph_transitivity()` or `graph_equivalency()`,
#'   depending on whether the object is a one-mode or two-mode object, respectively.
#' @export
graph_clustering <- function(object) {
  .Deprecated("graph_transitivity")
  if(is_twomode(object)){
    graph_equivalency(object)
  } else graph_transitivity(object)
}

#' @describeIn defunct Deprecated on 2021-10-26.
#' Returns `group_triad_census()`
#' @export
cluster_triad_census <- function(object, clusters) {
  .Deprecated("group_triad_census")
  group_triad_census(object, clusters)
}

#' @describeIn defunct Deprecated on 2021-10-26.
ggraphgrid <- function(x, algorithm = c("kk", "fr")) {
  .Deprecated("autographr(x, 'frgrid'")
  if(algorithm == "fr") autographr(x, "frgrid")
  if(algorithm == "kk") autographr(x, "kkgrid")
}

#' @describeIn defunct Deprecated on 2021-11-08.
#' Returns `test_random()`
#' @export
test_cug <- function(object, FUN, ..., nSim = 1000) {
  .Deprecated("test_random")
  test_random(object, FUN, ..., nSim)
}

#' @describeIn defunct Deprecated on 2021-11-10.
#' Returns `graph_dims()`
#' @export
graph_dimensions <- function(object) {
  .Deprecated("graph_dims")
  graph_dims(object)
}

#' @describeIn defunct Deprecated on 2021-11-16.
#' Returns `network_reg()`
#' @export
netlm <- function(formula, data, ...) {
  .Deprecated("network_reg")
  network_reg(formula, data, ...)
}

#' @describeIn defunct Deprecated on 2022-01-24.
#' This function is deprecated and its functionality is included in the more
#' general purpose `autographr()` function. Please refer to its documentation
#' for more details about the new implementation.
#' @export
ggidentify <- function(object, node_measure, identify_function = max) {
  # Deprecating the function for the time being. --> defunct in the next minor?
  .Deprecated("autographr", package = "migraph",
              msg = paste("This function has been included in the",
                          "`autographr()` function. Please run",
                          "`autographr(object, node_measure,",
                          "identify_function)` instead.",
                          sep = " "),
              old = "ggidentify")
  # The function
  object <- as_tidygraph(object)
  measure <- node_measure(object)
  colord <- ifelse(measure == identify_function(measure),
                   "max", "other")
  # Generate output
  ggraph::ggraph(object) +
    ggplot2::theme_void() +
    ggraph::geom_edge_link() +
    ggraph::geom_node_point(aes(size = measure,
                                colour = colord)) +
    ggplot2::scale_color_manual(breaks = c("max", "other"),
                                values = c("red", "blue")) +
    ggplot2::theme(legend.position = "none")
}
