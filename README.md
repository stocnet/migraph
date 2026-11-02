
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

`{migraph}` helps new users install and learn social network analysis in
R. One install gives you the full set of stocnet packages for making,
manipulating, plotting, measuring, testing, and modelling networks, and
a set of interactive tutorials that teach you how to use them. All the
functions work with base R matrices and edgelists, and with
[`{igraph}`](https://igraph.org/r/), [`{network}`](https://statnet.org),
and [`{tidygraph}`](https://tidygraph.data-imaginist.com/index.html)
objects, for one-mode and two-mode (bipartite) networks.

- [Install](#install)
- [Stay current](#stay-current)
- [Learn](#learn)
- [The packages](#the-packages)
- [Data](#data)
- [Funding details](#funding-details)

## Install

Install `{migraph}` from CRAN. Open the R console and type:

``` r
install.packages("migraph")
```

Then load it:

``` r
library(migraph)
```

This also loads [`{manynet}`](https://stocnet.github.io/manynet/),
[`{autograph}`](https://stocnet.github.io/autograph/),
[`{netrics}`](https://stocnet.github.io/netrics/), and
[`{infernet}`](https://stocnet.github.io/infernet/), so you have all
their functions and data at once.

Some functions and tutorials use other packages for one or two specific
tasks. `{migraph}` asks to install these when you first need them. To
install them all now, type:

``` r
install.packages("migraph", dependencies = TRUE)
```

## Stay current

The stocnet packages change frequently, with fixes and new features.
When you load `{migraph}`, it tells you if a newer version of one of
these packages is available (it checks at most once a week). To check
now and install all the newest versions, type:

``` r
update_stocnet()
```

Do this before a class, a test, or the submission of a paper. For each
package, `update_stocnet()` installs the newer of the CRAN release and
the GitHub release.

## Learn

`{migraph}` gives you interactive `{learnr}` tutorials from all the
stocnet packages. Type `run_tute()` to see the list, and
`run_tute("manynet1")` (for example) to start one. `extract_tute()`
saves the code of a tutorial as an R script. We suggest that you do the
tutorials in this sequence:

| Package   | Name       | Title                     |
|:----------|:-----------|:--------------------------|
| migraph   | tutorial0  | Intro to R                |
| manynet   | manynet1   | Making Network Data       |
| manynet   | manynet2   | Manipulating Network Data |
| autograph | autograph1 | Visualising Networks      |
| netrics   | netrics1   | Centrality                |
| netrics   | netrics2   | Cohesion and Community    |
| netrics   | netrics3   | Position and Equivalence  |
| netrics   | netrics4   | Topology and Resilience   |
| infernet  | infernet1  | Diversity and Regression  |
| infernet  | infernet2  | Diffusion and Learning    |
| infernet  | infernet3  | Modelling with ERGMs      |

## The packages

| Package | What it does | For example |
|----|----|----|
| [`{manynet}`](https://stocnet.github.io/manynet/) | makes, imports, coerces, and manipulates network data | `as_igraph()`, `to_undirected()`, `generate_random()` |
| [`{autograph}`](https://stocnet.github.io/autograph/) | plots networks and the results of analyses | `graphr()`, `plot()` |
| [`{netrics}`](https://stocnet.github.io/netrics/) | measures and marks nodes, ties, and networks | `node_by_degree()`, `net_by_density()` |
| [`{infernet}`](https://stocnet.github.io/infernet/) | tests and models networks: CUG and QAP tests, MRQAP, and diffusions | `test_random()`, `estimate_mrqap()`, `play_diffusions()` |
| `{migraph}` | loads and updates all of these, and holds the tutorials and the data of the book | `run_tute()`, `update_stocnet()` |

## Data

<img style="border:10px solid white;" src="https://jameshollway.com/media/9781108833509pvs01.jpg" align="left" alt="Cover image of the book Multimodal Political Networks" width="125"/>

`{migraph}` is the software companion to the book:

> David Knoke, Mario Diani, James Hollway, and Dimitris Christopoulos
> (2021) [*Multimodal Political
> Networks*](https://www.cambridge.org/core/books/multimodal-political-networks/43EE8C192A1B0DCD65B4D9B9A7842128).
> Cambridge University Press: Cambridge.

It includes most of the datasets in the book: `mpn_bristol`, `mpn_cow`,
`mpn_cow_igo`, `mpn_cow_trade`, `mpn_DE_1990`, `mpn_DE_2008`,
`mpn_elite_mex`, `mpn_elite_usa_advice`, `mpn_elite_usa_money`,
`mpn_evs_deu`, `mpn_evs_gbr`, `mpn_evs_ita`, `mpn_IT_1990`,
`mpn_IT_2008`, `mpn_ryanair`, `mpn_senate`, `mpn_senate_dem`,
`mpn_senate_over`, `mpn_senate_rep`, `mpn_UK_1990`, `mpn_UK_2008`. The
stocnet packages together implement most of the methods in the book.

<br clear="left"/>

## Funding details

Most work on this package has been funded by the Swiss National Science
Foundation (SNSF) [Grant Number
188976](https://data.snf.ch/grants/grant/188976): “Power and Networks
and the Rate of Change in Institutional Complexes” (PANARCHIC).
