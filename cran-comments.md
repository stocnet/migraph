## Test environments

* local R installation, aarch64-apple-darwin23, R 4.6.0
* macOS 15.7.7 (on Github), R 4.6.0
* Microsoft Windows Server 2025 10.0.26100 (on Github), R 4.6.0
* Ubuntu 24.04.4 (on Github), R 4.6.0

## R CMD check results

0 errors | 0 warnings | 0 notes

- Updated manynet dependency to 1.2.1 to fix reverse dependency issue
- Updated netrics dependency to 0.3.1 to fix reverse dependency issue
- Checked with CRAN versions of dependencies
  - Only issue was CRAN versions of manynet and netrics built under R v4.6.1,
  whereas this package was built locally under R v4.6.0, 
  though this should not be a problem for CRAN submission.
