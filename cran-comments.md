## Test environments

* local R installation, aarch64-apple-darwin23, R 4.6.1
* macOS 26.4 (on Github), R 4.6.1
* Microsoft Windows Server 2025 10.0.26100 (on Github), R 4.6.1
* Ubuntu 24.04.4 (on Github), R 4.6.1

## R CMD check results

0 errors | 0 warnings | 0 notes

This version also treats the rare model_tests issue that could occur under some random seeds,
and lead to an intermittent error on e.g. r-release-linux-x86_64 recently.
