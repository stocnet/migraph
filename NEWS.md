# migraph 0.5.0

2021-01-29

## Analysis

* Re-added `netlm()` for performing linear regression for multimodal network data
  * Closed #76 by changing `netlm()` to accept a formula-based input
  * Closed #77 by adding `print.summary.netlm()` for `netlm()` regressions 
* Closed #31 by adding `centrality_eigenvector()` for one- and two-mode networks
* Closed #32 by readding centralization functions for one- and two-mode networks
  * `centralisation_degree()` for degree centralization
  * `centralisation_closeness()` for closeness centralization
  * `centralisation_betweenness()` for betweenness centralization
* Re-added `clustering()` for calculating (see Knoke et al 2021): 
  * transitivity on one-mode networks
  * shared four-cycles on two-mode networks
  * congruent four-cycles on three-mode networks
* Re-added `constraint()` for calculating Burt's constraint measure for one- and two-mode networks 

## Manipulation

* Added functions for class conversion between migraph-consistent graph formats
 * `as_matrix()` function to coerce objects into an adjacency or incidence matrix class
 * `as_igraph()` function  to coerce objects into an `{igraph}` graph class
 * `as_tidygraph()` function to coerce objects into an `{tidygraph}` tbl_graph class
* Added `create_star()` function to create a star bipartite graph

# migraph 0.4.1

2021-01-11

## Package

* pkgdown now deploys after release
* Reexported a number of `{igraph}` and `{tidygraph}` functions for internal use
* Completed some `convert_` and `project_` documentation

## Data

* Updated mpn_ data source references

## Analysis

* Added centrality measures that take (and if necessary return) matrix, igraph, or tidygraph objects, and offer a correct normalization for two-mode networks
  * Added `centrality_degree()`
  * Added `centrality_closeness()`
  * Added `centrality_betweenness()`

# migraph 0.4.0

2021-01-08

## Package

* Package name change from roctopus to `{migraph}`
  * Closed #50 with new logo
* Now builds Linux binary too

## Manipulation

* Added `project_rows()` and `project_cols()` to make it easier to project two-mode networks in different formats (matrix, igraph, tidygraph) into projected versions in the same format
* Closed #30 with conversion from different data frame formats, e.g. weighted and unweighted edgelists, into an incidence matrix with `as_incidence_matrix()`

## Data

* Renamed data related to the book "Multimodal Political Networks" with "mpn_" prefix

# roctopus 0.3.0

2020-11-06

## Data

* Data creation updated for `{tidygraph}` defaults
  * Renamed `create_lattice()` to `create_chain()` to avoid conflicts with `tidygraph::create_lattice()`
  * Renamed `create_poles()` to `create_silos()`
  * Renamed `create_random()` to `play_twomode()` to avoid conflicts with `tidygraph::play_bipartite()`
  * Added export options for all `create_` and `play_` functions as `tbl_graph` (default), `igraph`, and base matrix
  * Updated tests for new `create_` and `play_` function names
* Packaged data updated for `{tidygraph}` defaults
  * Renamed packaged data from book to `mpn_`

# roctopus 0.2.6

2020-08-18

## Analysis

* Renamed `twomode_netlm()` to `netlm2()`
* Fixed various printing issues with `netlm2()`

## Package

* Added tests for `netlm2()`
* Added tests for `create_random()`

## Data

* Added `opensecrets` data
* Added `powerelite` data

# roctopus 0.2.5

2020-07-21

## Package

* Renamed data documentation scripts
* Added tests for most `create_()` functions
* Renamed test scripts and removed `context()` declaration
* `create_nest()` now exports matrix object

# roctopus 0.2.4

2020-07-20

## Package

* Added `row_project()` and `col_project()` functions to make it easier to remember project directions

# roctopus 0.2.3

2020-07-19

## Data

* Added `mexicanpower` dataset
* Added `senate112` datasets
* Updated `evs` datasets to matrices

## Package

* Added `df_to_mat()` function for converting regular adjacency and incidence data frames into matrices

# roctopus 0.2.1

2020-07-17

## Data

* Added `ryanair` dataset from Christopoulos 2006

## Package

* Split workflow into pull-request and merge/push prompted actions
* Added codecov calculation to workflows

# roctopus 0.2.0

2020-07-03

## Data

* Added `evs` datasets from Knoke et al 2020 

# roctopus 0.1.0

2020-06-30

## Data

* Added `bristol` dataset from Knoke et al 2020

# roctopus 0.0.4

2018-12-20

## Package

* Added hex sticker
* Updated README with more detailed installation information
* Pkgdown exports to https://jhollway.bitbucket.io/roctopus/

## Analysis

* Added `twomode_modularity()` to calculate modularity in two-mode networks

## Visualization

* Added `plot_multilevel()` that rotates a force-directed `igraph` plot in three dimensions to reveal multilevel structure
* Added `plot_globalnet()` to map a multilevel network on to a javascript, rotatable global

# roctopus 0.0.3

2018-08-25

## Analysis

* Added `twomode_smallworld()` to calculate observed/expected clustering, observed/expected path-length, and the observed/expected clustering ratio
* Added `twomode_2x2()` to identify dominance and coherence values for networks over time
* Updated `twomode_coherence()` to allow for introduction of second-mode attributes
* Renamed `twomode_fragmentation()` to `twomode_components()`

## Visualisation

* Added `plot_2x2()` to plot values through a two-by-two matrix

# roctopus 0.0.2

2018-08-14

## Package

* Renamed package to `roctopus`
* Added two-mode @family tag to documentation

## Analysis

* Added `twomode_fragmentation()` to calculate number of components in two-mode networks and identify their membership
* Added `twomode_dominance()` to allow an nodal attribute to be passed to the function to weight the centralization measure
* Added `twomode_coherence()` to calculate Jaccard similarity

## Visualisation

* Added `plot_twomode()`, which wraps `plot(igraph::graph_from_incidence_matrix())` with some useful defaults
    - coloured grayscale by default, with green/blue option
    - shaped circles and squares by default

# roctopus 0.0.1

2018-07-30

## Package

* Initialised package
* Added `README.md` file with instructions on how to install package
* Added `LICENSE` file and pointed to bug/issue tracker
* Added a `NEWS.md` file to track changes to the package

## Analysis

* Added `twomode_clustering()` to calculate percentage of three-paths closed by four-paths
* Added `twomode_lattice()` to create two-mode lattices
* Added `twomode_centralization_degree()` to calculate degree centralization in two-mode networks, for rows, columns, or both
* Added `twomode_centralization_between()` to calculate betweenness centralization in two-mode networks
* Added `twomode_constraint()` to calculate network constraint in two-mode networks
* Added `arrange.vars()` to rearrange variables by position
