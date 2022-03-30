#' Functions that have been renamed, superseded, or are no longer working
#' 
#' `r lifecycle::badge("deprecated")`
#' @name defunct
#' @keywords internal
NULL

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

#' @describeIn defunct Deprecated on 2022-03-23.
#' This function is deprecated and its functionality is included in the more
#' general purpose `autographr()` function. Please refer to its documentation
#' for more details about the new implementation.
#' @export
ggdistrib <- function(object, node_measure){
  .Deprecated("plot.measure", package = "migraph",
              msg = paste("This function has been converted into a",
                          "`plot()` method for a 'measure' class object.", 
                          "Please pass an object resulting from a `node_()`",
                          "function to `plot()` to achieve the same result.",
                          sep = " "),
              old = "ggdistrib")
}

#' @describeIn defunct Deprecated on 2022-03-23.
#' @export
project_rows <- function(object){
  .Deprecated("to_mode1", package = "migraph",
              old = "project_rows")
  to_mode1(object)
}

#' @describeIn defunct Deprecated on 2022-03-23.
#' @export
project_cols <- function(object){
  .Deprecated("to_mode2", package = "migraph",
              old = "project_cols")
  to_mode2(object)
}

#' @describeIn defunct Deprecated on 2022-03-29.
#' @export
mutate_edges <- function(object, object2, attr_name){
  .Deprecated("join_edges", package = "migraph",
              old = "mutate_edges")
  join_edges(object = object, object2 = object2, 
             attr_name = attr_name)
}
