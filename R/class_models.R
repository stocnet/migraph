# The tidy() and glance() methods for netlm, netlogit, ergm, and sienaFit
# results, the print methods for netlm and netlogit, and the predict()
# methods for netlm, netlogit, and netreg, are in {infernet}.
# {infernet} is attached with migraph, so tidy(), glance(), and test_gof() come
# from there directly, and are not re-exported here.

#' @importFrom infernet tidy glance
#' @export
summary.sienaFit <- function(object, ...){
  cat("# Fitted model results\n")
  print(tidy(object))
  cat("\n# Model summary statistics\n")
  print(glance(object))
}
