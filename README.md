
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
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7076396.svg)](https://doi.org/10.5281/zenodo.7076396)
<!-- see https://zenodo.org/record/7076396 -->
<!-- ![GitHub All Releases](https://img.shields.io/github/downloads/snlab-ch/migraph/total) -->
<!-- badges: end -->

## About the package

The most commonly used R packages available for network analysis, such
as `{igraph}` or `{sna}`, are mainly oriented around directed or
undirected one-mode networks. But researchers are increasingly
interested in analysing multimodal (one-, two-, or three-mode),
multilevel (connected multimodal networks), or multilayer (multiplex or
signed) networks. Existing procedures typically involve ‘projecting’
them into one-mode networks so that they can be used with those tools,
but thereby potentially losing important structural information, or
require one or more other specific packages. Translating between
packages various syntaxes and expectations can introduce significant
transaction costs though, driving confusion, inefficiencies, and errors.

`{migraph}` builds upon `{manynet}` to offer smart solutions to these
problems. It includes functions for marking and measuring networks and
their nodes and ties, identifying motifs and memberships in them, and
modelling these networks or simulating processes such as diffusion upon
them. Based on `{manynet}`, every function works for any compatible
network format - from base R matrices or edgelists as data frames,
[`{igraph}`](https://igraph.org/r/), [`{network}`](https://statnet.org),
or [`{tidygraph}`](https://tidygraph.data-imaginist.com/index.html)
objects. This means it is compatible with your existing workflow, is
extensible by other packages, and uses the most efficient algorithm
available for each task.

<img style="border:10px solid white;" src="https://jameshollway.com/media/9781108833509pvs01.jpg" align="left" width="125"/>

The package is intended as a software companion to the book:

> David Knoke, Mario Diani, James Hollway, and Dimitris Christopoulos
> (2021) [*Multimodal Political
> Networks*](https://www.cambridge.org/core/books/multimodal-political-networks/43EE8C192A1B0DCD65B4D9B9A7842128).
> Cambridge University Press: Cambridge.

Most datasets used in the book are included in this package, and the
package implements most methods discussed in the book. Since many of
theses datasets and routines are discussed and analysed more there, if
you like the package please check out the book, and vice versa.

## How does migraph help?

`{migraph}` includes five special groups of functions, each with their
own pretty `print()` and `plot()` methods: marks, measures, memberships,
motifs, and models.

`{migraph}` uses a common syntax to help new and experienced network
analysts find the right function and use it correctly. All `network_*()`
functions return a value for the network/graph or for each mode in the
network. All `node_*()` functions return values for each node or vertex
in the network. And all `tie_*()` functions return values for each tie
or edge in the network. Functions are given intuitive and succinct names
that avoid conflicts with existing function names wherever possible. All
results are normalised by default, facilitating comparison.

### Marks and Measures

`{migraph}`’s `*is_*()` functions offer fast logical tests of various
properties. Whereas `is_*()` returns a single logical value for the
network, `node_is_*()` returns a logical vector the length of the number
of nodes in the network, and `tie_is_*()` returns a logical vector the
length of the number of ties in the network.

- `node_is_core()`, `node_is_cutpoint()`, `node_is_exposed()`,
  `node_is_fold()`, `node_is_infected()`, `node_is_isolate()`,
  `node_is_latent()`, `node_is_max()`, `node_is_mentor()`,
  `node_is_min()`, `node_is_random()`, `node_is_recovered()`
- `tie_is_bridge()`, `tie_is_loop()`, `tie_is_max()`, `tie_is_min()`,
  `tie_is_multiple()`, `tie_is_reciprocated()`

The `*is_max()` and `*is_min()` functions are used to identify the
maximum or minimum, respectively, node or tie according to some measure
(see below).

`{migraph}` also offers a large and growing smorgasbord of measures that
can be used at the node, tie, and network level. Each recognises whether
the network is directed or undirected, weighted or unweighted, one-mode
or two-mode. All return normalized values wherever possible, though this
can be overrided. Here are some examples:

- *Centrality*: `node_degree()`, `node_closeness()`,
  `node_betweenness()`, and `node_eigenvector()`
- *Centralization*: `network_degree()`, `network_closeness()`,
  `network_betweenness()`, and `network_eigenvector()`
- *Cohesion*: `network_density()`, `network_reciprocity()`,
  `network_transitivity()`, `network_equivalency()`, and
  `network_congruency()`
- *Connectedness*: `network_components()`, `network_cohesion()`,
  `network_adhesion()`, `network_diameter()`, `network_length()`
- *Diversity*: `network_diversity()`, `network_homophily()`,
  `network_assortativity()`, `node_diversity()`, `node_homophily()`,
  `node_assortativity()`, `node_richness()`
- *Innovation*: e.g. `node_redundancy()`, `node_effsize()`,
  `node_efficiency()`, `node_constraint()`, `node_hierarchy()`
- *Topological features*: e.g. `network_core()`, `network_factions()`,
  `network_modularity()`, `network_smallworld()`, `network_balance()`

Please explore [the list of
functions](https://snlab-ch.github.io/migraph/reference/index.html) to
find out more.

### Motifs and Memberships

The package also include functions for returning various censuses at the
network or node level, e.g.:

- `network_brokerage_census()`, `network_dyad_census()`,
  `network_mixed_census()`, `network_triad_census()`
- `node_brokerage_census()`, `node_path_census()`, `node_quad_census()`,
  `node_tie_census()`, `node_triad_census()`

These can be analysed alone, or used as a profile for establishing
equivalence. `{migraph}` offers both HCA and CONCOR algorithms, as well
as elbow, silhouette, and strict methods for *k*-cluster selection.

- `node_automorphic_equivalence()`, `node_equivalence()`,
  `node_regular_equivalence()`, `node_structural_equivalence()`

`{migraph}` also includes functions for establishing membership on other
bases, such as typical community detection algorithms, as well as
component and core-periphery partitioning algorithms.

### Models

All measures can be tested against conditional uniform graph (CUG) or
quadratic assignment procedure (QAP) distributions using:

- `test_gof()`, `test_permutation()`, `test_random()`

Hypotheses can also be tested within multivariate models via multiple
(linear or logistic) regression QAP:

- `network_reg()`

`{migraph}` is the only package that offers these testing frameworks for
two-mode networks as well as one-mode networks.

Lastly, `{migraph}` also includes functions for simulating diffusion or
learning processes over a given network:

- `play_diffusion()`, `play_diffusions()`, `play_learning()`,
  `play_segregation()`

The diffusion models include not only SI and threshold models, but also
SIS, SIR, SIRS, SEIR, and SEIRS.

## Installation

### Stable

The easiest way to install the latest stable version of `{migraph}` is
via CRAN. Simply open the R console and enter:[^1]

`install.packages('migraph')`

You can then begin to use `{migraph}` by loading the package:

`library(migraph)`

This will load any required packages and make the data contained within
the package available.

### Development

For the latest development version, for slightly earlier access to new
features or for testing, you may wish to download and install the
binaries from Github or install from source locally.

The latest binary releases for all major OSes – Windows, Mac, and Linux
– can be found
[here](https://github.com/snlab-ch/migraph/releases/latest). Download
the appropriate binary for your operating system, and install using an
adapted version of the following commands:

- For Windows:
  `install.packages("~/Downloads/migraph_winOS.zip", repos = NULL)`
- For Mac:
  `install.packages("~/Downloads/migraph_macOS.tgz", repos = NULL)`
- For Unix:
  `install.packages("~/Downloads/migraph_linuxOS.tar.gz", repos = NULL)`

To install from source the latest main version of `{migraph}` from
Github, please install the `{remotes}` or `{devtools}` package from CRAN
and then:

- For latest stable version:
  `remotes::install_github("snlab-ch/migraph")`
- For latest development version:
  `remotes::install_github("snlab-ch/migraph@develop")`

### Tutorials

Together with `{manynet}`, this package makes available interactive
`{learnr}` tutorials. The easiest way to access the tutorials is via
`run_tute()`. If no tutorial name is provided, the function will return
a list of tutorials currently available in either package:

``` r
library(migraph)
run_tute()
#> # A tibble: 9 × 3
#>   package name      title        
#>   <chr>   <chr>     <chr>        
#> 1 manynet tutorial0 Intro to R   
#> 2 manynet tutorial1 Data         
#> 3 manynet tutorial2 Visualisation
#> 4 migraph tutorial3 Centrality   
#> 5 migraph tutorial4 Community    
#> 6 migraph tutorial5 Position     
#> 7 migraph tutorial6 Topology     
#> 8 migraph tutorial7 Diffusion    
#> 9 migraph tutorial8 Regression
# run_tute("tutorial5")
```

For more details on the `{learnr}` package, see
[here](https://rstudio.github.io/learnr/).

## Relationship to other packages

`{migraph}` draws together, updates, and builds upon many functions
currently available in other excellent R packages such as
[`{bipartite}`](https://github.com/biometry/bipartite),
[`{multinet}`](https://CRAN.R-project.org/package=multinet),
[`{tnet}`](https://toreopsahl.com/tnet/), and
[`{xUCINET}`](https://sites.google.com/view/asnr-2022/xucinet?authuser=0).

## Funding details

Most work on this package has been funded by the Swiss National Science
Foundation (SNSF) [Grant Number
188976](https://data.snf.ch/grants/grant/188976): “Power and Networks
and the Rate of Change in Institutional Complexes” (PANARCHIC).

[^1]: Macs with Macports installed may also install from the command
    line [using Macports](https://ports.macports.org/port/R-migraph/).
