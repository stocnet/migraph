
# roctopus <img src="man/figures/logo.png" align="right" width="150"/>

<!-- badges: start -->
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
![GitHub release (latest by date)](https://img.shields.io/github/v/release/jhollway/roctopus)
![GitHub Release Date](https://img.shields.io/github/release-date/jhollway/roctopus)
![GitHub issues](https://img.shields.io/github/issues-raw/jhollway/roctopus)
[![HitCount](http://hits.dwyl.com/jhollway/roctopus.svg)](http://hits.dwyl.com/jhollway/roctopus)
[![Codecov test coverage](https://codecov.io/gh/jhollway/roctopus/branch/main/graph/badge.svg)](https://codecov.io/gh/jhollway/roctopus?branch=main)
<!-- ![GitHub All Releases](https://img.shields.io/github/downloads/jhollway/roctopus/total) -->
<!-- badges: end -->

## About the package

This package presents functions for analysing multimodal and multilevel networks.
The package is intended as a software companion to the forthcoming book:

> David Knoke, Mario Diani, James Hollway, and Dimitris Christopoulos (2021) *Multimodal Political Networks*. Cambridge University Press: Cambridge.

The package is offered as a complement to existing R packages for network analysis.
It can analyse data in base formats such as matrices and (data frame) edgelists,
but also leverages [`{igraph}`](https://igraph.org/r/) and is consistent with a [`{tidygraph}`](https://tidygraph.data-imaginist.com/index.html) workflow.
It draws together, updates, and builds upon many functions currently available in
other excellent R packages such as 
[`{bipartite}`](https://github.com/biometry/bipartite), 
[`{multinet}`](https://cran.r-project.org/web/packages/multinet/multinet.pdf), 
[`{netmem}`](https://github.com/anespinosa/netmem), 
and [`{tnet}`](https://toreopsahl.com/tnet/),
and implements many additional features currently only available outside the R ecosystem
in packages such as [**UCINET**](https://sites.google.com/site/ucinetsoftware/downloads).

Please explore [the website](https://jhollway.github.io/roctopus/) to find out more.
Some of these functions are also relied on for the [Topological Typology](https://jhollway.shinyapps.io/TopoTypo/) app.

## Installation

### From binary

Perhaps the easiest way to install `{roctopus}` is by installing a compiled binary.
Binaries for all major OSes -- Windows, Mac, and Linux -- 
can be found by clicking on the latest release [here](https://github.com/snlab-nl/rsiena/releases/latest).
Download the appropriate binary for your operating system,
and install using an adapted version of the following commands:

- For Windows: `install.packages("~/Downloads/roctopus_winOS.zip", repos = NULL)`
- For Mac: `install.packages("~/Downloads/roctopus_macOS.tgz", repos = NULL)`
- For Unix: `install.packages("~/Downloads/roctopus_linuxOS.tar.gz", repos = NULL)`

### From source

To install from source the latest main version of `{roctopus}` from Github, 
please install the `{remotes}` package from CRAN and then enter into the console:

- For latest stable version: `remotes::install_github("jhollway/roctopus")`
- For latest development version: `remotes::install_github("jhollway/roctopus@develop")`

