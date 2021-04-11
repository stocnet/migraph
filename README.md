
# migraph <img src="man/figures/logo.png" align="right" width="150"/>

<!-- badges: start -->
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#maturing)
![GitHub release (latest by date)](https://img.shields.io/github/v/release/snlab-ch/migraph)
![GitHub Release Date](https://img.shields.io/github/release-date/snlab-ch/migraph)
<!-- ![GitHub issues](https://img.shields.io/github/issues-raw/snlab-ch/migraph) -->
<!-- [![HitCount](http://hits.dwyl.com/snlab-ch/migraph.svg)](http://hits.dwyl.com/snlab-ch/migraph) -->
[![Codecov test coverage](https://codecov.io/gh/snlab-ch/migraph/branch/main/graph/badge.svg)](https://codecov.io/gh/snlab-ch/migraph?branch=main)
[![CodeFactor](https://www.codefactor.io/repository/github/snlab-ch/migraph/badge)](https://www.codefactor.io/repository/github/snlab-ch/migraph)
[![CII Best Practices](https://bestpractices.coreinfrastructure.org/projects/4559/badge)](https://bestpractices.coreinfrastructure.org/projects/4559)
<!-- ![GitHub All Releases](https://img.shields.io/github/downloads/snlab-ch/migraph/total) -->
<!-- badges: end -->

## About the package

This package presents functions for analysing multimodal and multilevel networks.
The package is intended as a software companion to the forthcoming book:

> David Knoke, Mario Diani, James Hollway, and Dimitris Christopoulos (2021) [*Multimodal Political Networks*](https://www.cambridge.org/core/books/multimodal-political-networks/43EE8C192A1B0DCD65B4D9B9A7842128).
Cambridge University Press: Cambridge.

The package is offered as a complement to existing R packages for network analysis.
It can analyse data in base formats such as matrices and (data frame) edgelists,
but can also work with [`{igraph}`](https://igraph.org/r/) and [`{network}`](http://statnet.org) objects,
and is consistent with a [`{tidygraph}`](https://tidygraph.data-imaginist.com/index.html) workflow.

Please explore [the website](https://snlab-ch.github.io/migraph/) to find out more.

## Installation

### From binary

Perhaps the easiest way to install `{migraph}` is by installing a compiled binary.
Binaries for all major OSes -- Windows, Mac, and Linux -- 
can be found by clicking on the latest release [here](https://github.com/snlab-ch/migraph/releases/latest).
Download the appropriate binary for your operating system,
and install using an adapted version of the following commands:

- For Windows: `install.packages("~/Downloads/migraph_winOS.zip", repos = NULL)`
- For Mac: `install.packages("~/Downloads/migraph_macOS.tgz", repos = NULL)`
- For Unix: `install.packages("~/Downloads/migraph_linuxOS.tar.gz", repos = NULL)`

### From source

To install from source the latest main version of `{migraph}` from Github, 
please install the `{remotes}` package from CRAN and then enter into the console:

- For latest stable version: `remotes::install_github("jhollway/migraph")`
- For latest development version: `remotes::install_github("jhollway/migraph@develop")`

## Relationship to other packages

It draws together, updates, and builds upon many functions currently available in
other excellent R packages such as 
[`{bipartite}`](https://github.com/biometry/bipartite), 
[`{multinet}`](https://CRAN.R-project.org/package=multinet), 
[`{netmem}`](https://github.com/anespinosa/netmem), 
and [`{tnet}`](https://toreopsahl.com/tnet/),
and implements many additional features currently only available outside the R ecosystem
in packages such as [**UCINET**](https://sites.google.com/site/ucinetsoftware/downloads).
