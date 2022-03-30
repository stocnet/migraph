
<!-- README.md is generated from README.Rmd. Please edit that file -->

# migraph <img src="man/figures/logo.png" align="right" width="150"/>

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#maturing)
![CRAN/METACRAN](https://img.shields.io/cran/v/migraph) ![GitHub release
(latest by
date)](https://img.shields.io/github/v/release/snlab-ch/migraph)
![GitHub Release
Date](https://img.shields.io/github/release-date/snlab-ch/migraph)
[![Codecov test
coverage](https://codecov.io/gh/snlab-ch/migraph/branch/main/graph/badge.svg)](https://app.codecov.io/gh/snlab-ch/migraph?branch=main)
[![CodeFactor](https://www.codefactor.io/repository/github/snlab-ch/migraph/badge)](https://www.codefactor.io/repository/github/snlab-ch/migraph)
[![CII Best
Practices](https://bestpractices.coreinfrastructure.org/projects/4559/badge)](https://bestpractices.coreinfrastructure.org/projects/4559)
<!-- ![GitHub All Releases](https://img.shields.io/github/downloads/snlab-ch/migraph/total) -->
<!-- badges: end -->

## About the package

This package extends existing network analysis packages for analysing
multimodal and multilevel networks. The package is intended as a
software companion to the book:

> David Knoke, Mario Diani, James Hollway, and Dimitris Christopoulos
> (2021) [*Multimodal Political
> Networks*](https://www.cambridge.org/core/books/multimodal-political-networks/43EE8C192A1B0DCD65B4D9B9A7842128).
> Cambridge University Press: Cambridge.

## How does migraph help?

### Works with your current network analysis workflow

The package is offered as a complement to existing R packages for
network analysis and thus tries to work well with whatever format
network objects you are already working with. All `{migraph}` measures
and models work with data in base formats:

-   adjacency and incidence *matrices*
-   one-mode and two-mode *data frame* edgelists

as well as with objects constructed from the following packages:

-   [`{igraph}`](https://igraph.org/r/)
-   [`{network}`](http://statnet.org)
-   [`{tidygraph}`](https://tidygraph.data-imaginist.com/index.html)

`{migraph}`’s `as_*()` functions can be used to translate objects from
one of the above classes into any other, and include:

-   as\_edgelist(), as\_igraph(), as\_matrix(), as\_network(),
    as\_tidygraph()

These functions are designed to be as intuitive and lossless as
possible, outperforming many other class-coercion packages.

#### Manipulation

Regardless of which class object you are working with, the same syntax
can be used to work with and manipulate your data.

`{migraph}`’s `is_*()` functions offer fast logical tests of network
properties, e.g.:

-   is\_acyclic(), is\_bipartite(), is\_complex(), is\_connected(),
    is\_directed(), is\_edgelist(), is\_graph(), is\_labelled(),
    is\_migraph(), is\_multiplex(), is\_signed(), is\_twomode(),
    is\_uniplex(), is\_weighted()

`{migraph}`’s `to_*()` functions can be used on any class object to
transform networks into networks with other properties, e.g.:

-   to\_main\_component(), to\_mode1(), to\_mode2(), to\_multilevel(),
    to\_named(), to\_onemode(), to\_simplex(), to\_undirected(),
    to\_uniplex(), to\_unnamed(), to\_unsigned(), to\_unweighted()

#### Making

`{migraph}` includes algorithms for making both one-mode and two-mode
networks with particular properties. The `create_*` group of functions
create networks with a particular structure, e.g.:

-   create\_complete(), create\_components(), create\_empty(),
    create\_lattice(), create\_ring(), create\_star(), create\_tree()

The `generate_*` group of functions generate networks from particular
generative mechanisms, e.g.:

-   generate\_permutation(), generate\_random(), generate\_scalefree(),
    generate\_smallworld()

`{migraph}` includes a number of prominent network datasets, especially
multimodal and multiplex examples for demonstrating more advanced
methods.

-   mpn\_bristol, mpn\_DE\_1990, mpn\_DE\_2008, mpn\_DemSxP,
    mpn\_elite\_mex, mpn\_elite\_usa\_advice, mpn\_elite\_usa\_money,
    mpn\_IT\_1990, mpn\_IT\_2008, mpn\_OverSxP, mpn\_RepSxP,
    mpn\_ryanair, mpn\_UK\_1990, mpn\_UK\_2008
-   ison\_adolescent\_friends, ison\_algebra\_class, ison\_bb, ison\_bm,
    ison\_brandes, ison\_eies, ison\_karateka,
    ison\_marvel\_relationships, ison\_marvel\_teams, ison\_mb,
    ison\_mm, ison\_southern\_women

`{migraph}` can also import and export to Excel edgelists and nodelists,
[UCINET](http://www.analytictech.com/archive/ucinet.htm) and
[Pajek](http://mrvar.fdv.uni-lj.si/pajek/) files, e.g.:

-   read\_edgelist(), read\_nodelist(), read\_pajek(), read\_ucinet()
-   write\_edgelist(), write\_nodelist(), write\_pajek(),
    write\_ucinet()

### Extends your current network analysis workflow

`{migraph}` offers a range of measures and models with sensible
defaults. Many wrap existing functions in common packages for use with
one-mode networks, but extend these to treat and/or normalise for
two-mode (and sometimes three-mode) networks correctly. Functions are
given intuitive and succinct names that avoid conflicts with existing
function names wherever possible.

#### Measures

-   Centrality: `node_degree()`, `node_closeness()`,
    `node_betweenness()`, and `node_eigenvector()`
-   Centralization: `graph_degree()`, `graph_closeness()`,
    `graph_betweenness()`, and `graph_eigenvector()`
-   Cohesion: `graph_density()`, `graph_reciprocity()`,
    `graph_transitivity()`, `graph_equivalency()`, and
    `graph_congruency()`
-   Censuses: `node_tie_census()`, `node_dyad_census()`,
    `node_triad_census()`, `node_mixed_census()`, `node_quad_census()`,
    and `graph_triad_census()`
-   Other measures: e.g. `node_constraint()`, `graph_smallworld()`

#### Models

-   Blockmodelling: `blockmodel_concor()`,
    `cluster_structural_equivalence()`, `cluster_regular_equivalence()`
-   Linear and logistic regression for networks (via MRQAP):
    `network_reg()`

#### Visualization

-   `autographr()` for plotting graphs with sensible defaults based on
    their properties
-   New layouts: e.g. `layout_tbl_graph_frgrid()` for snapping
    Fruchterman-Reingold to a grid
-   Class-based plots: e.g. `plot.blockmodel()`

Please explore [the list of
functions](https://snlab-ch.github.io/migraph/reference/index.html) to
find out more.

## Installation

### Stable

The easiest way to install the latest stable version of `{migraph}` is
via CRAN. Simply open the R console and enter:

`install.packages('migraph')`

You can then begin to use `{migraph}` by loading the package:

`library(migraph)`

This will load any required packages and make the data contained within
the package available. The version from CRAN also has all the vignettes
built and included. You can check them out with:

`vignettes(package = "migraph")`

### Development

For the latest development version, for slightly earlier access to new
features or for testing, you may wish to download and install the
binaries from Github or install from source locally.

The latest binary releases for all major OSes – Windows, Mac, and Linux
– can be found
[here](https://github.com/snlab-ch/migraph/releases/latest). Download
the appropriate binary for your operating system, and install using an
adapted version of the following commands:

-   For Windows:
    `install.packages("~/Downloads/migraph_winOS.zip", repos = NULL)`
-   For Mac:
    `install.packages("~/Downloads/migraph_macOS.tgz", repos = NULL)`
-   For Unix:
    `install.packages("~/Downloads/migraph_linuxOS.tar.gz", repos = NULL)`

To install from source the latest main version of `{migraph}` from
Github, please install the `{remotes}` or `{devtools}` package from CRAN
and then:

-   For latest stable version:
    `remotes::install_github("snlab-ch/migraph")`
-   For latest development version:
    `remotes::install_github("snlab-ch/migraph@develop")`

## Relationship to other packages

It draws together, updates, and builds upon many functions currently
available in other excellent R packages such as
[`{bipartite}`](https://github.com/biometry/bipartite),
[`{multinet}`](https://CRAN.R-project.org/package=multinet), and
[`{tnet}`](https://toreopsahl.com/tnet/), and implements many additional
features currently only available outside the R ecosystem in packages
such as
[**UCINET**](https://sites.google.com/site/ucinetsoftware/download?authuser=0).
