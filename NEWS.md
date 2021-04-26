# migraph 0.6.3

2021-04-26

## Package

* Updated README
  * Updated installation instructions for CRAN
  * Added package functions overview
* Added `CITATION` details
  
## Classes

* Separated coercion (previously conversion) and manipulation
* Added some more inter-class coercion tests
* Fixed bug in how `as_network()` sometimes coerced two-mode networks into much larger dimension matrices
* Added more `is_` tests for class-independent property tests
  * Added `is_weighted()`
  * Added `is_directed()`
  * Added `is_labelled()`

## Data

* Added @csteglich 's `read_ucinet()` and `write_ucinet()` functions
  * `read_ucinet()` offers a file-picker when file path unknown
  * `read_ucinet()` now imports to an igraph-class object by default,
  with an argument to allow other alternatives
  * `write_ucinet()` works with all migraph-compatible objects
* Updated `mpn_bristol` documentation
* Added `create_star()` function
  * Added in-star/out-star option via `directed = ` argument
  * Updated `create_` documentation
* Renamed `sample_affiliation()` to `generate_random()`
  * Rewrote `generate_random()` to be able to generate random one- or two-mode networks
  * Updated documentation

## Models

* Added test for `print.blockmodel()`

# migraph 0.6.2

2021-04-13

## Package

* Reran `usethis::use_mit_license("James Hollway")`. MIT License file now contains only the standard two lines.
* Removed `\dontrun` from examples. `netlm()` now runs in <5 seconds.
* Fixed missing website item

# migraph 0.6.1

2021-04-11

## Package

* Closed #21 by elaborating DESCRIPTION file in preparation for CRAN submission
* Updated several old URLs in documentation 

## Classes

* Closed #85 by adding `as_network()` to coerce objects into network class
* Modified other coercion functions to also work with network class objects

# migraph 0.6.0

2021-03-03

## Package

* Moved package's Github repository from `jhollway/` to `snlab-ch/` organisation
* Trimmed some package dependencies and added others

## Data

* Elaborated documentation for the remainder of the datasets
  * Now all datasets in this package are titled with whether they are 
  one-mode, two-mode, or three-mode
  
## Measures

* Fixed bug in `graph_degree()` where data was hard-coded in

## Models

* Closed #18 by adding `blockmodel_concor()` for employing the CONCOR algorithm 
  to blockmodel both one-mode and two-mode networks
  * Added a new print method for "blockmodel"-class objects based on the
    `print.blockmodel()` method in the `{sna}` package that also prints
    blockmodel results for two-mode networks consistently
  * Added a new plot method for "blockmodel"-class objects that leverages
    `{ggplot2}` for pretty plotting and that better inherits names from
    the underlying object

# migraph 0.5.0

2021-02-06

## Package

* Closed #81 by making `{migraph}` depend on R versions 4.0 or above
* Updated PR template

## Classes

* Added functions for class conversion between migraph-consistent graph formats
 * `as_matrix()` function to coerce objects into an adjacency or incidence matrix class
 * `as_igraph()` function  to coerce objects into an `{igraph}` graph class
 * `as_tidygraph()` function to coerce objects into an `{tidygraph}` tbl_graph class
* Closed #79 by adding `is_twomode()` function to check whether network is two-mode on all object types

## Data

* Renamed several datasets and elaborated their documentation
  * `mpn_mexicanpower` was renamed to `mpn_elite_mex`
  * `mpn_powerelite` was renamed to `mpn_elite_usa_advice`
  * `mpn_opensecrets` was renamed to `mpn_elite_usa_money`
* Reconstructed several creation functions to take universal (one-mode/two-mode) input: 
specifying `n = 5` creates a one-mode network, while specifying `n = c(5, 5)` creates a two-mode network
  * Added `create_empty()`
  * Added `create_complete()`
  * Closed #65 by extending `create_ring()` to create rings of varying breadth
  * Closed #66 by extending `create_components()` (renamed from `create_silos()`) to create networks with
  varying numbers of components
  * Added `sample_affiliation()` for random two-mode networks
  * Removed `create_match()` and `create_nest()`

## Measures

* Renamed `centrality_` functions with `node_` prefix and ensured they all also wrapped one-mode measures
  * `centrality_degree()` renamed to `node_degree()`
  * `centrality_closeness()` renamed to `node_closeness()`
  * `centrality_betweenness()` renamed to `node_betweenness()`
  * Closed #31 by adding `node_eigenvector()`
* Re-added `node_constraint()` for calculating Burt's constraint measure for one- and two-mode networks 
* Re-added `node_smallworld()` for calculating Watts-Strogatz measure of small-worldness for two-mode networks 
* Closed #32 by re-adding centralization functions for one- and two-mode networks
  * `graph_degree()` for degree centralization
  * `graph_closeness()` for closeness centralization
  * `graph_betweenness()` for betweenness centralization
* Re-added `graph_clustering()` for calculating (see Knoke et al 2021): 
  * transitivity on one-mode networks
  * shared four-cycles on two-mode networks
  * congruent four-cycles on three-mode networks

## Models

* Re-added `netlm()` for performing linear regression for multimodal network data
  * Closed #76 by changing `netlm()` to accept a formula-based input
  * Closed #77 by adding `print.summary.netlm()` for `netlm()` regressions
  
## Plotting

* Closed #82 by re-adding a version `plot.igraph()` with sensible defaults for two-mode networks

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
