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

# nocov end

# Moved to infernet ####

# The tests, the regression, and the diffusion simulations moved to {infernet}.
# The functions that kept their name there (`test_random()`,
# `test_configuration()`, `test_permutation()`, `play_diffusions()`) need no
# shim: {infernet} is attached with {migraph}, and reads an old `strategy`
# argument as a control. The functions below were renamed or changed syntax, so
# each maps its old arguments to {infernet}'s, and warns once a session that it
# is deprecated. `strategy` becomes a control, and `verbose` is dropped.

.migraph_state <- new.env(parent = emptyenv())

.moved_to_infernet <- function(old, new = old) {
  if (isTRUE(.migraph_state[[old]])) return(invisible())
  .migraph_state[[old]] <- TRUE
  .Deprecated(paste0("infernet::", new), package = "migraph",
              msg = paste0("`", old, "()` has moved to {infernet}, ",
                           "and is deprecated in {migraph}.\n",
                           "Use `infernet::", new, "()` instead."))
}

#' @describeIn defunct Deprecated on 2026-10-27.
#'   Calls `infernet::estimate_mrqap()`, with `method = "qap"` as
#'   `permute = "predictor"` and `method = "qapy"` as `permute = "outcome"`.
#'   It returns a `netreg` rather than a `netlm` or `netlogit`.
#' @param formula,.data,times,strategy,verbose As in `{infernet}`'s functions,
#'   where `strategy` is a control and `verbose` is not used.
#' @param method `"qap"` or `"qapy"`.
#' @export
net_regression <- function(formula, .data,
                           method = c("qap", "qapy"),
                           times = 1000,
                           strategy = "sequential",
                           verbose = FALSE) {
  .moved_to_infernet("net_regression", "estimate_mrqap")
  method <- match.arg(method)
  permute <- c(qap = "predictor", qapy = "outcome")[[method]]
  infernet::estimate_mrqap(formula, .data, times = times,
                           control = list(permute = permute,
                                          strategy = strategy))
}

#' @describeIn defunct Deprecated on 2026-10-27.
#'   Calls `infernet::test_distributions()`, which compares two `diffs_model`
#'   objects from `play_diffusions()` rather than two single diffusions.
#' @param diff_model1,diff_model2 Two `diffs_model` objects.
#' @export
test_distribution <- function(diff_model1, diff_model2) {
  .moved_to_infernet("test_distribution", "test_distributions")
  infernet::test_distributions(diff_model1, diff_model2)
}

#' @describeIn defunct Deprecated on 2026-10-27.
#'   Calls `infernet::test_gof()`, with the simulations first.
#' @param diff_model A `diff_model` object, or a network with a diffusion.
#' @param diff_models A `diffs_model` object, from `play_diffusions()`.
#' @export
test_fit <- function(diff_model, diff_models) {
  .moved_to_infernet("test_fit", "test_gof")
  if (manynet::is_graph(diff_model))
    diff_model <- manynet::as_diffusion(diff_model)
  infernet::test_gof(diff_models, diff_model)
}
