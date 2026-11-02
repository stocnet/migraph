#' Update the stocnet packages
#'
#' @description
#'   `update_stocnet()` installs the newest versions of
#'   `{migraph}`, `{manynet}`, `{autograph}`, `{netrics}`, and `{infernet}`.
#'   For each package, it takes the newer of the release on CRAN and
#'   the release on GitHub (the main branch).
#'   CRAN gives a binary install and needs no compiler,
#'   so when the two have the same version, it installs from CRAN.
#'
#'   Use it to get the newest fixes and features
#'   before a class, a test, or the submission of a paper.
#'   On attach, `{migraph}` tells you when one of these packages is behind,
#'   but it checks at most once a week.
#'   `update_stocnet()` always checks again.
#'
#' @param ask Logical.
#'   If `TRUE` (the default in an interactive session),
#'   it lists the packages that are behind and asks before it installs them.
#'   If `FALSE`, it installs them without a question.
#' @return Invisibly, a named list with one element for each package that was
#'   behind, which gives the newer `version` and its `source`
#'   ("CRAN" or "GitHub").
#'   An empty list if all packages are current or the check could not run.
#' @examples
#' # This changes the packages in your library, so it is not run here.
#' \dontrun{
#' update_stocnet()
#' }
#' @export
update_stocnet <- function(ask = interactive()) {
  # nocov start
  # Under a bare Rscript no repository is set, so the CRAN check would not run.
  # The user asked to update, so use the cloud mirror for this call only.
  repos <- getOption("repos")
  if (is.null(repos) || !length(repos) || any(repos == "@CRAN@")) {
    old <- options(repos = c(CRAN = "https://cloud.r-project.org"))
    on.exit(options(old), add = TRUE)
  }

  manynet::snet_info("Checking CRAN and GitHub for the newest versions...")
  cran <- snet_cran_versions()
  # Longer than on attach: the user is waiting for this answer.
  gh <- snet_github_versions(timeout = 10)
  if (is.null(cran) && is.null(gh)) {
    manynet::snet_warn("Could not reach CRAN or GitHub. Check your connection, and try again.")
    return(invisible(list()))
  }
  snet_write_cache(cran, gh)

  behind <- snet_check_versions(cran = cran, gh = gh)
  if (!length(behind)) {
    manynet::snet_success("All stocnet packages are current.")
    return(invisible(behind))
  }

  sources <- snet_split_sources(behind)
  vers <- vapply(behind, function(x) x$version, character(1))
  manynet::snet_info(paste0(
    "Newer versions: ",
    paste0(names(behind), " ", vers,
           " (", vapply(behind, function(x) x$source, character(1)), ")",
           collapse = ", "), "."))

  if (isTRUE(ask)) {
    yes <- utils::askYesNo("Install them now?")
    if (!isTRUE(yes)) return(invisible(behind))
  }

  if (length(sources$cran)) {
    utils::install.packages(sources$cran)
  }
  if (length(sources$github)) {
    thisRequires("remotes")
    for (p in sources$github) {
      # The stocnet dependencies come first in the loop, so leave the other
      # dependencies as they are.
      remotes::install_github(paste0("stocnet/", p), upgrade = "never")
    }
  }

  # The cache holds what the repositories offered, which is still true, but
  # clear it so that the next attach checks against the new installs at once.
  unlink(snet_cache_file())
  manynet::snet_success("Done. Restart R to use the new versions.")
  invisible(behind)
  # nocov end
}
