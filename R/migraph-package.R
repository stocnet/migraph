# nocov start

#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

# defining global variables more centrally
utils::globalVariables(c(".data", "obs", "fin","n","S","I","t","sim","time","value","conf.low","conf.high",
                         "name"))

# Suppress R CMD check note
# Namespace in Imports field not imported from: PKG
#   All declared Imports should be used.
#' @importFrom autograph ag_base
#' @importFrom netrics net_by_heterophily
ignore_unused_imports <- function() {
  # This function exists only to reference autograph::ag_base and suppress R CMD check notes about unused imports.
  autograph::ag_base
  netrics::net_by_heterophily
  NULL
}

# nocov end