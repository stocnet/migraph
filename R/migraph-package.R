# nocov start

#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

# Helper function for checking and downloading packages
thisRequires <- function(pkgname){
  if (!requireNamespace(pkgname, quietly = TRUE)) {
    if(utils::askYesNo(msg = paste("The", pkgname, 
                                   "package is required to run this function. Would you like to install", pkgname, "from CRAN?"))) {
      utils::install.packages(pkgname)
    } else {
      stop(paste("Please install", pkgname, "from CRAN to run this function."))
    }
  }
}

# defining global variables more centrally
utils::globalVariables(c(".data", "obs", "fin","n","sim","time","value","conf.low","conf.high"))

# Suppress R CMD check note
# Namespace in Imports field not imported from: PKG
#   All declared Imports should be used.
#' @importFrom autograph ag_base
ignore_unused_imports <- function() {
  # This function exists only to reference autograph::ag_base and suppress R CMD check notes about unused imports.
  autograph::ag_base
  NULL
}

# nocov end