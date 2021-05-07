## Test environments

* local R installation, x86_64-apple-darwin17.0, R 4.0.5
* Mac OS X 10.15.7 (on Github), R 4.0.4
* Microsoft Windows Server 2019 10.0.17763 (on Github), R 4.0.4
* Ubuntu 20.04.2 (on Github), R 4.0.4

## R CMD check results

0 errors | 0 warnings | 0 notes

* Responds to email from Brian Ripley/CRAN team (2021-04-26)
  * Removed unused package dependencies (R6, ggraph)
  * Avoided M1mac check issue by dropping the sensitive test
