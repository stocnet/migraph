#' Update the versions of the stocnet packages
#'
#' @description
#'   `update_versions()` installs the newest versions of
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
#'   `update_versions()` always checks again.
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
#' update_versions()
#' }
#' @export
update_versions <- function(ask = interactive()) {
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
    manynet::snet_requires("remotes")
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

#' Table the versions of the stocnet packages
#'
#' @description
#'   `table_versions()` gives a table of
#'   `{migraph}`, `{manynet}`, `{autograph}`, `{netrics}`, and `{infernet}`,
#'   with the version that is installed,
#'   the newest version on CRAN and on GitHub (the main branch),
#'   and the library that R loads each package from.
#'
#'   Use it to see which packages are behind before you run `update_versions()`.
#'   If you have a problem with your installation,
#'   send this table to your teacher or with your issue on GitHub.
#'
#'   The `status` column gives one of these values:
#'
#'   - "up to date": no newer version is on CRAN or GitHub.
#'   - "newer on CRAN" or "newer on GitHub":
#'   `update_versions()` installs the newer version.
#'   - "not installed": the package is not in any library.
#'   - "not checked": CRAN and GitHub could not be reached.
#'
#'   A package can be in more than one library.
#'   Then an update can go to one library while R still loads the other.
#'   The `libraries` column counts the libraries that hold the package,
#'   and the `library` column gives the one that R uses.
#' @return A tibble of class `migraphVersions`,
#'   with one row for each package and the columns
#'   `package`, `installed`, `cran`, `github`, `status`, `library`, and `libraries`.
#' @seealso [update_versions()] to install the newer versions.
#' @examples
#' \donttest{
#' table_versions()
#' }
#' @export
table_versions <- function() {
  # nocov start
  # Under a bare Rscript no repository is set, so the CRAN check would not run.
  repos <- getOption("repos")
  if (is.null(repos) || !length(repos) || any(repos == "@CRAN@")) {
    old <- options(repos = c(CRAN = "https://cloud.r-project.org"))
    on.exit(options(old), add = TRUE)
  }
  manynet::snet_info("Checking CRAN and GitHub for the newest versions...")
  cran <- snet_cran_versions()
  # The user is waiting for this answer, so allow as long as `update_versions()`.
  gh <- snet_github_versions(timeout = 10)
  if (!is.null(cran) || !is.null(gh)) snet_write_cache(cran, gh)
  out <- snet_version_table(cran = cran, gh = gh,
                            installed = snet_installed_versions(),
                            libraries = snet_installed_libraries())
  class(out) <- c("migraphVersions", class(out))
  out
  # nocov end
}

#' @export
print.migraphVersions <- function(x, ...) {
  NextMethod()
  # After a user selects or renames columns, the advice below no longer applies.
  if (!all(c("package", "status", "libraries") %in% names(x))) return(invisible(x))
  behind <- x$package[startsWith(x$status, "newer")]
  absent <- x$package[x$status == "not installed"]
  multi <- x$package[!is.na(x$libraries) & x$libraries > 1]
  if (nrow(x) && all(x$status %in% c("not checked", "not installed")))
    manynet::snet_warn("Could not reach CRAN or GitHub. Check your connection, and try again.")
  if (length(behind))
    manynet::snet_info(paste0("Newer versions of ", paste(behind, collapse = ", "),
                              " are available. Install them with `update_versions()`."))
  if (length(absent))
    manynet::snet_info(paste0("Install the missing packages with `install.packages(c(",
                              paste0('"', absent, '"', collapse = ", "), "))`."))
  if (length(multi))
    manynet::snet_warn(paste0(paste(multi, collapse = ", "),
                              if (length(multi) > 1) " are" else " is",
                              " in more than one library. R loads the one in the `library` column."))
  invisible(x)
}
