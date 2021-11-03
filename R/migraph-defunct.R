#' Defunct functions
#' 
#' `r lifecycle::badge("deprecated")`
#' @name defunct
#' @keywords internal
NULL

#' @rdname defunct
#' @description Deprecated on 2021-10-26.
#' @return `group_triad_census()`
#' @export
cluster_triad_census <- function(object, clusters) {
  .Deprecated("group_triad_census")
  group_triad_census(object, clusters)
}

