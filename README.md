
<!-- README.md is generated from README.Rmd. Please edit that file -->

# migraph <img src="man/figures/logo.png" alt="migraph logo" align="right" width="150"/>

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#maturing)
![CRAN/METACRAN](https://img.shields.io/cran/v/migraph) ![GitHub release
(latest by
date)](https://img.shields.io/github/v/release/stocnet/migraph) ![GitHub
Release
Date](https://img.shields.io/github/release-date/stocnet/migraph)
[![Codecov test
coverage](https://codecov.io/gh/stocnet/migraph/branch/main/graph/badge.svg)](https://app.codecov.io/gh/stocnet/migraph?branch=main)
<!-- [![CodeFactor](https://www.codefactor.io/repository/github/stocnet/migraph/badge)](https://www.codefactor.io/repository/github/stocnet/migraph) -->
[![CII Best
Practices](https://bestpractices.coreinfrastructure.org/projects/4559/badge)](https://bestpractices.coreinfrastructure.org/projects/4559)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7076396.svg)](https://doi.org/10.5281/zenodo.7076396)
<!-- see https://zenodo.org/record/7076396 -->
<!-- ![GitHub All Releases](https://img.shields.io/github/downloads/stocnet/migraph/total) -->
<!-- badges: end -->

## About the package

Most commonly used R packages available for network analysis, such as
`{igraph}` or `{sna}`, are mainly oriented around directed or undirected
one-mode networks. But researchers are increasingly interested in
analysing multimodal (one-, two-, or three-mode), multilevel (connected
multimodal networks), or multilayer (multiplex or signed) networks.
Existing procedures typically involve ‘projecting’ them into one-mode
networks so that they can be used with those tools, but thereby
potentially losing important structural information, or require one or
more other specific packages. Translating between packages various
syntaxes and expectations can introduce significant transaction costs
though, driving confusion, inefficiencies, and errors. `{migraph}`
includes functions for inferential network analysis.

`{migraph}` builds upon
[`{manynet}`](https://stocnet.github.io/manynet/) to offer smart
solutions to these problems. Since it is based on `{manynet}`, every
function works for any compatible network format - from base R matrices
or edgelists as data frames, [`{igraph}`](https://igraph.org/r/),
[`{network}`](https://statnet.org), or
[`{tidygraph}`](https://tidygraph.data-imaginist.com/index.html)
objects. This means it is compatible with your existing workflow, is
extensible by other packages, and uses the most efficient algorithm
available for each task.

- [About the package](#about-the-package)
  - [Package background](#package-background)
- [How does migraph help?](#how-does-migraph-help)
- [Tutorials](#tutorials)
- [Installation](#installation)
  - [Stable](#stable)
  - [Development](#development)
- [Relationship to other packages](#relationship-to-other-packages)
- [Funding details](#funding-details)

### Package background

<img style="border:10px solid white;" src="https://jameshollway.com/media/9781108833509pvs01.jpg" align="left" alt="Cover image of the book Multimodal Political Networks" width="125"/>

The package is intended as a software companion to the book:

> David Knoke, Mario Diani, James Hollway, and Dimitris Christopoulos
> (2021) [*Multimodal Political
> Networks*](https://www.cambridge.org/core/books/multimodal-political-networks/43EE8C192A1B0DCD65B4D9B9A7842128).
> Cambridge University Press: Cambridge.

Most datasets used in the book are included in this package, and
`{manynet}` and `{migraph}` together implement most methods discussed in
the book. Since many of theses datasets and routines are discussed and
analysed more there, if you like the package(s) please check out the
book, and vice versa.

## How does migraph help?

`{migraph}` allows the testing of `{manynet}` measures against
conditional uniform graph (CUG) or quadratic assignment procedure (QAP)
distributions using:

- `test_configuration()`, `test_distribution()`, `test_fit()`,
  `test_gof()`, `test_permutation()`, `test_random()`

<img src="https://www.jameshollway.com/post/migraph/tests-2.png" alt="Plot showing the results of a QAP test"/>

Hypotheses can also be tested within multivariate models via multiple
(linear or logistic) regression QAP:

- `network_reg()`

<img src="https://www.jameshollway.com/post/migraph/regression-1.png" alt="A violin plot showing the results of an MRQAP"/>

`{migraph}` is the only package that offers these testing frameworks for
two-mode networks as well as one-mode networks.

## Tutorials

Together with `{manynet}`, this package makes available interactive
`{learnr}` tutorials. The easiest way to access the tutorials is via
`run_tute()`. If no tutorial name is provided, the function will return
a list of tutorials currently available in either package:

``` r
library(migraph)
run_tute()
#> Checking tutorials in stocnet packages ■■■■■■■■■■■■■■■■ 50% | …
#> # A tibble: 9 × 3
#>   package name      title                   
#>   <chr>   <chr>     <chr>                   
#> 1 manynet tutorial0 Intro to R              
#> 2 manynet tutorial1 Data                    
#> 3 manynet tutorial2 Visualisation           
#> 4 manynet tutorial3 Centrality              
#> 5 manynet tutorial4 Cohesion and Community  
#> 6 manynet tutorial5 Position and Equivalence
#> 7 manynet tutorial6 Topology and Resilience 
#> 8 manynet tutorial7 Diffusion and Learning  
#> 9 migraph tutorial8 Diversity and Regression
#> ℹ You can run one of these tutorials by typing e.g `run_tute('tutorial1')` or `run_tute('Data')` into the console.
# run_tute("tutorial5")
```

For more details on the `{learnr}` package, see
[here](https://rstudio.github.io/learnr/).

## Installation

### Stable

The easiest way to install the latest stable version of `{migraph}` is
via CRAN. Simply open the R console and enter:[^1]

`install.packages('migraph')`

You can then begin to use `{migraph}` by loading the package:

`library(migraph)`

This will load any required packages and make the data contained within
the package available.

`{migraph}` relies on some packages only for one or two rather specific
functions. By default these are not installed together with `{migraph}`,
but we make it easy to install them as and when needed for the first
time with a console prompt. If you would prefer not to encounter these
prompts, or plan to use the package for the first time through
tutorials, you can make sure all the dependencies are installed with:

`install.packages('migraph', dependencies = TRUE)`

### Development

For the latest development version, for slightly earlier access to new
features or for testing, you may wish to download and install the
binaries from Github or install from source locally.

The latest binary releases for all major OSes – Windows, Mac, and Linux
– can be found
[here](https://github.com/stocnet/migraph/releases/latest). Download the
appropriate binary for your operating system, and install using an
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
  `remotes::install_github("stocnet/migraph")`
- For latest development version:
  `remotes::install_github("stocnet/migraph@develop")`

### Other sources

Those using Mac computers may also install using Macports:

`sudo port install R-migraph`

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
