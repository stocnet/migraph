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
