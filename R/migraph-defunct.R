# nocov start

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

#' @describeIn defunct Deprecated on 2024-06-16.
#' @export
test_gof <- function(diff_model, diff_models) {
  .Deprecated("test_fit", package = "migraph",
              old = "test_gof")
  test_fit(diff_model, diff_models)
}

#' @describeIn defunct Deprecated on 2024-07-19.
#' @export
network_reg <- function(formula, .data,
                        method = c("qap","qapy"),
                        times = 1000,
                        strategy = "sequential",
                        verbose = FALSE) {
  .Deprecated("net_regression", package = "migraph",
              old = "network_reg")
  net_regression(formula, .data, method, times,
                 strategy, verbose)
}

# nocov end