
# migraph <img src="man/figures/logo.png" align="right" width="150"/>

<!-- badges: start -->
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#maturing)
![CRAN/METACRAN](https://img.shields.io/cran/v/migraph)
![GitHub release (latest by date)](https://img.shields.io/github/v/release/snlab-ch/migraph)
![GitHub Release Date](https://img.shields.io/github/release-date/snlab-ch/migraph)
[![Codecov test coverage](https://codecov.io/gh/snlab-ch/migraph/branch/main/graph/badge.svg)](https://app.codecov.io/gh/snlab-ch/migraph?branch=main)
[![CodeFactor](https://www.codefactor.io/repository/github/snlab-ch/migraph/badge)](https://www.codefactor.io/repository/github/snlab-ch/migraph)
[![CII Best Practices](https://bestpractices.coreinfrastructure.org/projects/4559/badge)](https://bestpractices.coreinfrastructure.org/projects/4559)
<!-- ![GitHub All Releases](https://img.shields.io/github/downloads/snlab-ch/migraph/total) -->
<!-- badges: end -->

## About the package

This package extends existing network analysis packages for analysing multimodal and multilevel networks.
The package is intended as a software companion to the book:

> David Knoke, Mario Diani, James Hollway, and Dimitris Christopoulos (2021) [*Multimodal Political Networks*](https://www.cambridge.org/core/books/multimodal-political-networks/43EE8C192A1B0DCD65B4D9B9A7842128).
Cambridge University Press: Cambridge.

## How does migraph help?

### Works with your current network analysis workflow

The package is offered as a complement to existing R packages for network analysis
and thus tries to work well with your existing analytic workflows and the classes invoked.
All `{migraph}` measures and models work with data in base formats:

- adjacency and incidence _matrices_
- one-mode and two-mode _data frame_ edgelists

as well as with objects from the following packages:

- [`{igraph}`](https://igraph.org/r/)
- [`{network}`](http://statnet.org)
- [`{tidygraph}`](https://tidygraph.data-imaginist.com/index.html)

`{migraph}` can also import and export to Excel edgelists and 
[UCINET](http://www.analytictech.com/archive/ucinet.htm) files.

### Extends your current network analysis workflow

`{migraph}` offers a range of measures and models with sensible defaults.
Many wrap existing functions in common packages for use with one-mode networks,
but extend these to treat and/or normalise for two-mode (and sometimes three-mode) networks correctly.
Functions are given intuitive and succinct names that avoid conflicts 
with existing function names wherever possible.

#### Manipulation

- Coercion between classes: `as_igraph()`, `as_tidygraph()`, `as_network()`, `as_edgelist()`, and `as_matrix()`
- Logical tests of properties: e.g. `is_twomode()`, `is_directed()`, `is_labelled()`, `is_weighted()`
- Transforming properties: `to_undirected()`, `to_unnamed()`, `to_unweighted()`, `to_onemode()`, and `to_main_component()`
- From two-mode to one-mode: `project_rows()` and `project_cols()`

#### Measures

- Centrality: `node_degree()`, `node_closeness()`, `node_betweenness()`, and `node_eigenvector()`
- Centralization: `graph_degree()`, `graph_closeness()`, `graph_betweenness()`, and `graph_eigenvector()`
- Cohesion: `graph_density()`, `graph_reciprocity()`, `graph_transitivity()`, `graph_equivalency()`, and `graph_congruency()`
- Censuses: `node_tie_census()`, `node_dyad_census()`, `node_triad_census()`, `node_mixed_census()`, `node_quad_census()`, and `graph_triad_census()`
- Other measures: e.g. `node_constraint()`, `node_smallworld()`

#### Models

- Blockmodelling: `blockmodel_concor()`, `cluster_structural_equivalence()`, `cluster_regular_equivalence()`
- Linear regression: `netlm()`

#### Visualization

- `autographr()` for plotting graphs with sensible defaults based on their properties
- New layouts: e.g. `layout_tbl_graph_frgrid()` for snapping Fruchterman-Reingold to a grid
- Class-based plots: e.g. `plot.blockmodel()`

Please explore [the list of functions](https://snlab-ch.github.io/migraph/reference/index.html) to find out more.

## Installation

### Stable

The easiest way to install the latest stable version of `{migraph}` is via CRAN.
Simply open the R console and enter:

`install.packages('migraph')`

You can then begin to use `{migraph}` by loading the package:

`library(migraph)`

This will load any required packages and make the data contained within the package available.
The version from CRAN also has all the vignettes built and included.
You can check them out with:

`vignettes(package = "migraph")`

### Development

For the latest development version, 
for slightly earlier access to new features or for testing,
you may wish to download and install the binaries from Github
or install from source locally.

The latest binary releases for all major OSes -- Windows, Mac, and Linux -- 
can be found [here](https://github.com/snlab-ch/migraph/releases/latest).
Download the appropriate binary for your operating system,
and install using an adapted version of the following commands:

- For Windows: `install.packages("~/Downloads/migraph_winOS.zip", repos = NULL)`
- For Mac: `install.packages("~/Downloads/migraph_macOS.tgz", repos = NULL)`
- For Unix: `install.packages("~/Downloads/migraph_linuxOS.tar.gz", repos = NULL)`

To install from source the latest main version of `{migraph}` from Github, 
please install the `{remotes}` or `{devtools}` package from CRAN and then:

- For latest stable version: `remotes::install_github("snlab-ch/migraph")`
- For latest development version: `remotes::install_github("snlab-ch/migraph@develop")`

## Relationship to other packages

It draws together, updates, and builds upon many functions currently available in
other excellent R packages such as 
[`{bipartite}`](https://github.com/biometry/bipartite), 
[`{multinet}`](https://CRAN.R-project.org/package=multinet), 
and [`{tnet}`](https://toreopsahl.com/tnet/),
and implements many additional features currently only available outside the R ecosystem
in packages such as [**UCINET**](https://sites.google.com/site/ucinetsoftware/download?authuser=0).
