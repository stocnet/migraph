## Test environments

* local R installation, aarch64-apple-darwin23, R 4.6.1
* macOS 26.4 (on Github), R 4.6.1
* Microsoft Windows Server 2025 10.0.26100 (on Github), R 4.6.1
* Ubuntu 24.04.4 (on Github), R 4.6.1

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependency check of netrics 1.0.1

The submission of `{netrics}` 1.0.1 reported one test failure in the CRAN version of `{migraph}`:

```
── Failure ('test-measure_over.R:9:3'): over_membership works ──
Expected `unname(unlist(c(res)))` to equal `c(0.490201713, NaN)`.
```

This version replaces that test with a fixed network and an explicit membership vector, 
so it no longer depends on any upstream clustering method.

This submission is therefore what unblocks `{netrics}` 1.0.1, 
and it deliberately does not require it. 
`{migraph}` declares `netrics (>= 0.4.0)`, the version currently on CRAN, 
so this package can be accepted before `{netrics}` 1.0.1 is. 
It checks cleanly against both:

| `{netrics}` | R CMD check |
|---|---|
| 0.4.0, the version on CRAN | 0 errors, 0 warnings, 0 notes |
| 1.0.1, the pending submission | 0 errors, 0 warnings, 0 notes |

`{migraph}` calls six `{netrics}` functions. 
All six are exported by both versions and return identical values.

## User filespace and internet access

On attach in interactive sessions only, 
this version checks whether the installed stocnet packages are outdated, 
and caches the result for seven days in `tools::R_user_dir("migraph", "cache")`. 
This replaces a check that previously ran on every attach in each of three dependencies, 
so it reduces both network use and startup time.

The check queries CRAN and the packages' public GitHub repositories. 
It is wrapped in `tryCatch()` with a short timeout, 
fails silently when offline or when no repository is configured, 
is skipped entirely in non-interactive sessions, 
and can be disabled with `options(snet_check_version = FALSE)`.
The package is fully functional if the cache directory is absent or unwritable.

