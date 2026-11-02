## Test environments

* local R installation, aarch64-apple-darwin23, R 4.6.1
* macOS 26.4 (on Github), R 4.6.1
* Microsoft Windows Server 2025 10.0.26100 (on Github), R 4.6.1
* Ubuntu 24.04.4 (on Github), R 4.6.1

## R CMD check results

0 errors | 0 warnings | 0 notes

## Changes in this version

This major version moves the tests, the network regression, and the diffusion
simulations to the new package 'infernet' (0.6.1, now on CRAN).
'migraph' now Depends on 'infernet', so that these functions are still
available to users who attach 'migraph'.
The functions that were renamed in 'infernet' remain here as deprecated shims.

## Reverse dependencies

There are 2 reverse dependencies on CRAN: 'autograph' and 'goldfish'.
Both have 'migraph' only in Suggests.
Neither calls a removed function in code that runs on CRAN.
