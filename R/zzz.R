# nocov start

# The stocnet packages used to each check CRAN for a newer version of themselves
# on attach. Since migraph Depends on manynet, autograph, and netrics, that meant
# three blocking round-trips per session. The check now lives here only, covers
# the whole stack at once, and is cached so it runs at most weekly.

snet_pkgs <- c("migraph", "manynet", "autograph", "netrics")

# How long to trust a cached result before checking again.
snet_check_interval <- 7

snet_cache_file <- function() {
  file.path(tools::R_user_dir("migraph", which = "cache"), "version-check.rds")
}

snet_read_cache <- function() {
  f <- snet_cache_file()
  if (!file.exists(f)) return(NULL)
  out <- tryCatch(readRDS(f), error = function(e) NULL)
  if (!is.list(out) || is.null(out$date) || !inherits(out$date, "Date")) return(NULL)
  if (as.numeric(Sys.Date() - out$date) >= snet_check_interval) return(NULL)
  out
}

snet_write_cache <- function(behind) {
  f <- snet_cache_file()
  # Failure to cache is not worth bothering the user about; the check simply
  # runs again next session.
  tryCatch({
    dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
    saveRDS(list(date = Sys.Date(), behind = behind), f)
  }, error = function(e) NULL, warning = function(w) NULL)
}

# Versions available on CRAN. Returns a named character vector, or NULL when no
# repository is configured (as under a bare Rscript) or CRAN is unreachable.
snet_cran_versions <- function() {
  repos <- getOption("repos")
  if (is.null(repos) || !length(repos) || any(repos == "@CRAN@")) return(NULL)
  tryCatch({
    ap <- utils::available.packages()
    have <- intersect(snet_pkgs, rownames(ap))
    if (!length(have)) return(NULL)
    out <- ap[have, "Version"]
    names(out) <- have
    out
  }, error = function(e) NULL, warning = function(w) NULL)
}

# Versions on the release branch of each GitHub repo. Much cheaper than the CRAN
# index: four ~2KB files from a CDN rather than the full package database.
snet_github_versions <- function() {
  tryCatch({
    # Generous: fetching all four takes about 0.4s on a working connection.
    old <- options(timeout = 2)
    on.exit(options(old), add = TRUE)
    out <- character()
    for (p in snet_pkgs) {
      url <- paste0("https://raw.githubusercontent.com/stocnet/", p,
                    "/main/DESCRIPTION")
      txt <- tryCatch(readLines(url, warn = FALSE), error = function(e) NULL)
      # If the first request fails the host is unreachable, so give up rather
      # than waiting out the timeout once per package. Attach should never
      # stall on a bad network.
      if (is.null(txt)) {
        if (!length(out)) return(NULL) else next
      }
      line <- grep("^Version:", txt, value = TRUE)
      if (!length(line)) next
      out[[p]] <- trimws(sub("^Version:", "", line[1]))
    }
    if (!length(out)) NULL else out
  }, error = function(e) NULL, warning = function(w) NULL)
}

# Which packages are behind, and where the newer version lives. When CRAN and
# GitHub agree, prefer CRAN: it is the binary install and needs no compiler.
snet_check_versions <- function() {
  cran <- snet_cran_versions()
  gh <- snet_github_versions()
  if (is.null(cran) && is.null(gh)) return(NULL)
  out <- list()
  for (p in snet_pkgs) {
    installed <- tryCatch(utils::packageVersion(p), error = function(e) NULL)
    if (is.null(installed)) next
    cv <- if (!is.null(cran) && p %in% names(cran)) utils::packageVersion(cran[[p]]) else NULL
    gv <- if (!is.null(gh) && p %in% names(gh)) utils::packageVersion(gh[[p]]) else NULL
    if (!is.null(cv) && cv > installed) {
      out[[p]] <- list(version = as.character(cv), source = "CRAN")
    } else if (!is.null(gv) && gv > installed) {
      out[[p]] <- list(version = as.character(gv), source = "GitHub")
    }
  }
  out
}

snet_report_outdated <- function(behind) {
  if (!length(behind)) return(invisible(NULL))
  pkgs <- names(behind)
  vers <- vapply(behind, function(x) x$version, character(1))
  from_cran <- pkgs[vapply(behind, function(x) x$source, character(1)) == "CRAN"]
  from_gh <- setdiff(pkgs, from_cran)

  packageStartupMessage(
    "Newer version", if (length(pkgs) > 1) "s" else "", " available: ",
    paste0(pkgs, " ", vers, collapse = ", "), ".")

  # Deliberately printed rather than prompted for. `utils::menu()` reads from
  # stdin, and when stdin is at EOF while `interactive()` is still TRUE it loops
  # forever, hanging the session on `library()`. Attach should never block on
  # input, so name the command and let the user run it.
  if (length(from_cran)) {
    packageStartupMessage("Update from CRAN with:\n  update.packages(c(",
                          paste0('"', from_cran, '"', collapse = ", "), "))")
  }
  if (length(from_gh)) {
    packageStartupMessage(
      "Not yet on CRAN. Install from GitHub with:\n  remotes::install_github(c(",
      paste0('"stocnet/', from_gh, '"', collapse = ", "), "))")
  }
  invisible(NULL)
}

.onAttach <- function(...) {

  if (!interactive()) return()
  if (!isTRUE(getOption("snet_check_version", TRUE))) return()

  cached <- snet_read_cache()
  if (!is.null(cached)) {
    snet_report_outdated(cached$behind)
    return(invisible(NULL))
  }

  behind <- snet_check_versions()
  # NULL means the check could not run (offline, no repo); don't cache that, so
  # it is retried next session rather than suppressed for a week.
  if (is.null(behind)) return(invisible(NULL))

  snet_write_cache(behind)
  snet_report_outdated(behind)

}

# nocov end
