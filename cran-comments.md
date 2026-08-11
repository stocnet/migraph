## Test environments

* local R installation, aarch64-apple-darwin23, R 4.6.1
* macOS 26.4 (on Github), R 4.6.1
* Microsoft Windows Server 2025 10.0.26100 (on Github), R 4.6.1
* Ubuntu 24.04.4 (on Github), R 4.6.1

## R CMD check results

0 errors | 0 warnings | 0 notes

## User filespace and internet access

On attach in interactive sessions only, this version checks whether the installed stocnet packages
are outdated, and caches the result for seven days in `tools::R_user_dir("migraph", "cache")`. This
replaces a check that previously ran on every attach in each of three dependencies, so it reduces
both network use and startup time.

The check queries CRAN and the packages' public GitHub repositories. It is wrapped in `tryCatch()`
with a short timeout, fails silently when offline or when no repository is configured, is skipped
entirely in non-interactive sessions, and can be disabled with `options(snet_check_version = FALSE)`.
The package is fully functional if the cache directory is absent or unwritable.
