#' Defunct functions
#' 
#' `r lifecycle::badge("deprecated")`
#' @name defunct
#' @keywords internal
NULL

#' @rdname defunct
#' @description Deprecated on 2021-10-18.
#' @return `graph_transitivity()` or `graph_equivalency()`,
#'   depending on whether the object is a one-mode or two-mode object, respectively.
#' @export
graph_clustering <- function(object) {
  .Deprecated("graph_transitivity")
  if(is_twomode(object)){
    graph_equivalency(object)
  } else graph_transitivity(object)
}

#' @rdname defunct
#' @description Deprecated on 2021-10-26.
#' @return `group_triad_census()`
#' @export
cluster_triad_census <- function(object, clusters) {
  .Deprecated("group_triad_census")
  group_triad_census(object, clusters)
}

#' @rdname defunct
#' @description Deprecated on 2021-11-08.
#' @return `test_random()`
#' @export
test_cug <- function(object, FUN, ..., nSim = 1000) {
  .Deprecated("test_random")
  test_random(object, FUN, ..., nSim)
}

