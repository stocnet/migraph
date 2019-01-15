[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

# roctopus <img src="man/figures/logo.png" width="240px" height="278px" align="right" />

## About the package

This package assembles functions for simulating, visualising, and analysing
multimode and multilevel networks.

Some of these functions are relied on for the [Topological Typology](https://jhollway.shinyapps.io/TopoTypo/).

## Relation to other software

In many ways, **roctopus** implements many of the functions 
currently available in **UCINET**,
but being an R package is platform independent 
and can be used alongside other R packages for network analysis
such as `igraph`, the `statnet` suite of packages, `RSiena` and `goldfish`.

While there are other packages available in R for two-mode analysis,
such as `tnet`, `bipartite`, and others,
these are not necessarily built for the purpose of *multimodal* network analysis.
Moreover, they generally implement some but not all of the range of
measures available for multimodal network analysis.

**roctopus** is part of the [**Suisse Suite**](https://jhollway.bitbucket.io/) 
of R packages for social and political network analysis and modelling.

## Installation

**roctopus** can be installed using the devtools package:

```R
devtools::install_bitbucket("jhollway/roctopus", auth_user = "name", password = "password")
```

where `"name"` and `"password"` are *your* bitbucket username and password. 
\(Note that this is currently not working for Windows versions of R/RStudio. We are looking into a solution.\)

You can email me for user access to the repository.


