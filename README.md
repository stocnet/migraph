
# roctopus <img src="inst/roctopus.png" align="right" width="150"/>

<!-- badges: start -->
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
![GitHub release (latest by date)](https://img.shields.io/github/v/release/jhollway/roctopus)
![GitHub Release Date](https://img.shields.io/github/release-date/jhollway/roctopus)
![GitHub issues](https://img.shields.io/github/issues-raw/jhollway/roctopus)
[![HitCount](http://hits.dwyl.com/jhollway/roctopus.svg)](http://hits.dwyl.com/jhollway/roctopus)
[![Codecov test coverage](https://codecov.io/gh/jhollway/roctopus/branch/main/graph/badge.svg)](https://codecov.io/gh/jhollway/roctopus?branch=main)
![GitHub All Releases](https://img.shields.io/github/downloads/jhollway/roctopus/total)
<!-- badges: end -->

## About the package

This package presents functions for analysing multimodal and multilevel networks.
The package is intended as a software companion to the forthcoming book:

> David Knoke, Mario Diani, James Hollway, and Dimitris Christopoulos (2021) *Multimodal Political Networks*. Cambridge University Press: Cambridge.

The package is offered as a complement to existing R packages for network analysis.
It can analyse data in base formats such as matrices and (data frame) edgelists,
but is also built upon `{igraph}` and consistent with a `{tidygraph}` workflow.
It draws together, updates, and builds upon many functions currently available in
other R packages such as `{tnet}`, `{bipartite}`, and `{multinet}`,
and implements many additional features currently only available outside the R ecosystem
in packages such as **UCINET**.

Please explore the website to find out more.
Some of these functions are also relied on for the [Topological Typology](https://jhollway.shinyapps.io/TopoTypo/).

## Installation

### From binary

Perhaps the easiest way to install `{roctopus}` is by installing a compiled binary.
Binaries for all major OSes -- Windows, Mac, and Linux -- 
can be found by clicking on the latest release [here](https://github.com/snlab-nl/rsiena/releases/latest).
Download the appropriate binary for your operating system,
and install using an adapted version of the following commands:

- For Windows: `install.packages("~/Downloads/roctopus_1.2.26.zip", repos = NULL)`
- For Mac: `install.packages("~/Downloads/roctopus_1.2.26.tgz", repos = NULL)`
- For Unix: `install.packages("~/Downloads/roctopus_1.2-26_R_x86_64-pc-linux-gnu.tar.gz", repos = NULL)`

### From source

To install from source the latest main version of `{roctopus}` from Github, 
please install the `{remotes}` package from CRAN and then enter into the console: 
`remotes::install_github("jhollway/roctopus")`.
The latest development version of `{roctopus}` can also be installed in a similar way: 
`remotes::install_github("jhollway/roctopus@develop")`.

