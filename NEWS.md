# migraph 0.8.13

## Modelling

* Closed #149 by adding extra column to node_tie_census in `cluster_structural_equivalence()` for isolates
  - Note that this renders all isolates structurally equivalent

# migraph 0.8.12

## Package
* Closed #168 by adding `{patchwork}` to suggested packages in DESCRIPTION
* Updated function reference page on website

## Manipulation
* Updated `add_` functions
  * Closed #178 by adding name to existing edges when further edges added in 
  `mutate_edges()`
  * Closed #179 by inferring an attribute vector is for one of the two modes 
  where possible in `add_node_attributes()`
* Added `is_` methods: `is_multiplex()`, `is_uniplex()`, `is_acyclic()`
* Added `edge_` functions to identify edges by properties: `edge_mutual()`, 
`edge_multiple()`, `edge_loop()`

# migraph 0.8.11

## Import and export
- Fixed #172 by removing redundant header argument in `read_nodelist()` and
  `read_edgelist()`

## Package
- Fixed #173 by extending `as_network()` method to convert correctly form
  an `{igraph}` to a `{network}` object.
- Removed `ggraphgrid()` documentation

# migraph 0.8.10

## Import and export
* Replaced xlsx dependency in `read_edgelist()` and `read_nodelist()` to readxl 
to avoid Java dependency
* Replaced xlsx dependency in `write_edgelist()` and `write_nodelist()` to avoid
Java dependency
  * Note that these functions will now export to .csv rather than .xlsx
  
## Manipulation
* Fixed direction recognition bug in `as_network()`, `as_igraph()`, and 
`is_directed()`

# migraph 0.8.9

## Package
* Closed #139 by adding vignette on importing and connecting data

## Import and export
* Added `read_` and `write_` functions and updated documentation
  * Closed #137 by adding `read_edgelist()` for importing edgelists from
  Excel and csv files
  * Closed #170 by adding `read_pajek()` for importing .net and .paj files
  * Added `write_edgelist()`, `write_nodelist`, `write_pajek()`, and 
  `write_ucinet()` for exporting into various file formats (Excel, csv, 
  Pajek, and UCINET)
  * Closed #140 by adding links to further data resources

## Manipulation
* Added `is_graph()` to check if an object is a graph or not
* Extended `as_network()` to retain attributes
* Fixed bugs in `as_` and `to_` functions
  * Fixed bug in `as_` functions to convert from dataframes instead of 
  tibbles
  * Fixed bug in conversion from network to igraph object in `as_igraph()` 
  function
  * Fixed bug in `to_undirected()` function to work with network objects
  * Fixed bug in `to_main_component()` function so that it retains vertex 
  attributes in network objects
* Added `edge_attribute()` to grab a named edge attribute from a graph/network
* Updated `to_unweighted()` to prevent conversion of network object into igraph 
object when deleting weights

## Measures
* Closed #143 by adding nodal summary by cluster function `summarise_statistics()`

## Modelling
* Fixed `network_reg()` example

## Visualisation
* Closed #117 by updating the node/edge/arrow size limits in `autographr()`

# migraph 0.8.8

## Package

- Added start to network linear model part of practical 7 vignette
- Thanks to @BBieri for adding many tests and working on igraph<->network interchange 

## Data

- Added `ison_eies` dataset for use in practical 7 vignette

## Manipulation

- The `as_matrix()` method for networks now works with two-mode and weighted networks
- The `as_igraph()` method for matrices now checks for weights independently of coercion
- The `as_igraph()` method for networks now works with two-mode and weighted networks
- The `as_network()` method for matrices now works with two-mode and weighted networks
- The `as_network()` method for edgelists, igraph, and tidygraphs now works with weighted networks
- Added `to_unnamed()` method for edge lists
- Added `to_simplex()` method for matrices
- Added `to_main_component()` method for networks
- Added `to_multilevel()` method for matrices
- `mutate_edges()` now coalesces rows of edges 

## Measures

- Fixed bug where clusters were not being reported in the correct order in `graph_blau_index()`

## Modelling

- Fixed one-mode bug with `generate_permutation()` and thus `test_permutation()`
- Renamed `netlm()` to `network_reg()` to avoid frustrating conflicts
  - `network_reg()` now accepts migraph-consistent objects
  - `network_reg()` now accepts formula terms such as `ego()`, `alter()`, and `same()`

# migraph 0.8.7

## Package

- Added new issue templates and refined the wording in existing templates
- Improved documentation across many help pages
- Closed #146 by adding vignette on homophily

## Data

- Added `generate_permutation()` which takes an object and returns an object
  with the edges permuted, but retaining all nodal attributes
- Made `generate_random()` also work with an existing object as input,
  in which it will return a random graph with the same dimensions and density
- Consolidated data scripts

## Manipulation

- Added `mutate_edges()` for adding new edges as attributes to existing edges
  in an object

## Measures

- Closed #159 by fixing bug in `graph_blau_index()`
- Closed #157 by fixing bug in `graph_ei_index()`
- Closed #156 and #158 by fixing bugs with `test_random()` (defunct `test_cug()`)

## Visualisation

- Closed #148 and #153 by making all `autographr()` arguments take variable names in
  quotation marks

# migraph 0.8.6

## Package

- Closed #75 by updating the README

## Manipulation

- Added some functions for grabbing key information from objects
  - `node_names()` for quickly accessing node labels
  - `node_attribute()` for quickly accessing a certain nodal attribute
  - `edge_weights()` for quickly accessing edge weights
  - `graph_nodes()` for quickly accessing a count of nodes in the graph, note that for two-mode networks this will be a vector of length 2
  - `graph_edges()` for quickly accessing a count of edges in the graph
  - `graph_dimensions()` is currently a copy of `graph_nodes()`
- Added some functions for adding key information to objects
  - `add_node_attributes()` for adding particular nodal attributes
  - `add_edge_attributes()` for adding edges from another graph
  - `copy_edge_attributes()` for copying all nodal attributes from another graph
- Improved twomode and weighted handling of several functions

## Measures

- Added diversity functions
  - `graph_blau_index()` for summarising diversity of an attribute in a network or group
  - `graph_ei_index()` for summarising diversity of an attribute over a network's ties

## Modelling

- Closed #119 by adding `node_quad_census()`, especially useful for two-mode blockmodelling
- Closed #95 and #120 by adding `graph_mixed_census()`
- Closed #97 by adding test functions
  - `test_random()` carries out a conditional uniform graph (CUG) test
  - `test_permutation()` carries out a quadratic assignment procedure (QAP) test

## Visualization

- Closed #135 by reexporting `aes()` from `{ggplot2}`
- Added `node_shape` option to `autographr()`

# migraph 0.8.5

## Package

- Updated various URLs in the vignettes to pass CRAN tests
- Reduce number of layout examples to avoid examples taking too long to run

# migraph 0.8.4

## Classes

- Closed #128 by adding `as_edgelist()` methods for converting other objects into edgelists
  - Note that this currently returns a tibble
- Using `to_unnamed()` on 'network' objects now operates on them directly
- Elaborated `to_` documentation significantly
- Fixed bug in `to_onemode()` that was tripping `blockmodel()` on networks that are already one-mode
- Added `is_connected()` to test whether network is connected, `method = ` argument can be specified as `weak` or `strong`

## Data

- Added `create_tree()` and `create_lattice()`, and made `create_star()` a bit faster for one-mode networks
- Added `generate_smallworld()` and `generate_scalefree()`, though only for one-mode networks currently

## Measures

- Added rounding to centralization measures, by default `=2`
- Closed #109 by adding centrality vignette

## Modelling

- Added `graph_dyad_census()` for more graph profile options
- Fixed bug with `blockmodel_concor()` when an object was of class 'igraph' but not 'tbl_graph'
- Fixed bug in how `blockmodel()` was treating two-mode networks
- Closed #116 by offering both `"elbow"` and `"strict"` methods for _k_-identification
  - Fixed bug in elbow method that biased heavily bipartitioned data
- Closed #131 by refactoring `ggidentify_clusters()` for speed 
  - Takes now roughly half the time (see issue for details)

## Visualization

- Added `ggdistrib()` for easy plotting of degree and other node score distributions
- Reexported `ggsave()`, `xlab()` and `ylab()` from `{ggplot2}` for easier plot annotation

# migraph 0.8.3

## Package

- Closed #108 by adding cohesion and community vignette

## Classes

- Fixed #122 by retaining edge weights from igraph in `as_matrix()` where available

## Measures

- Split `graph_equivalency()` into the same for two-mode networks and `graph_congruency() `for three-mode (two two-mode) networks
- Added option for `graph_reciprocity()` method
- Added `graph_components()` and `node_components()`

## Modelling

- Fixed #113 by retaining node labels through census functions
- Closed #114 by transposing `node_tie_census()` output so that it's consistent with `node_triad_census()` and future node_census functions
- Closed #121 by renaming `cluster_triad_census()` to `group_triad_census()`
- Added `group_tie_census()`

## Visualization

- Added option to `autographr()` for plotting convex/concave hulls
- Closed #124 by making `ggraphgrid()` a set of layout functions:
  - `layout_tbl_graph_frgrid()` or `autographr(object, "frgrid")` for snapping Fruchterman-Reingold to a grid
  - `layout_tbl_graph_kkgrid()` or `autographr(object, "kkgrid")` for snapping Kamada-Kawai to a grid
  - `layout_tbl_graph_gogrid()` or `autographr(object, "gogrid")` for snapping graph optimisation to a grid
  - `ggraphgrid()` has been deprecated

## Data

- Fixed some `ison_m182` documentation

# migraph 0.8.2

## Package

- Fixed CRAN package check dependencies bug where 'knitr' and 'rmarkdown' were listed as Imports without being used in the package

## Classes

- Fixed bug where bipartite edge lists were not being recognised as a twomode network by `as_igraph()`
- Fixed bug where `to_uniplex()` was not returning a weighted graph

## Models

- Fixed bug where `blockmodel()` was not retaining node names in all parts of the object structure

## Visualization

- Closed #107 by choosing better brewer pallette (though note this is not a very deep pallette with only 9 colors)

## Vignettes

- Expanded on the blockmodelling vignette with more intro, discussion, interpretation clues

# migraph 0.8.1

## Package

- Fixed codecov url bug
- Removed several package dependencies by moving `plot_releases()` to another package
- Made many dependencies more explicit
- Entire package 'linted'

## Classes

- Added `is_signed()` to logically test whether the network is a signed network
- Added `to_unsigned()` for extracting networks of either "positive" or "negative" ties from a signed network
- Added `tbl_graph` methods for all other `to_` functions
- Reexported `activate()` from `{tidygraph}`

## Visualisation

- Added sensible plotting defaults for signed networks in `autographr()`
- Removed `plot_releases()` from this package

## Measures

- Refactored `graph_balance()` to be much faster, following David Schoch's `{signnet}` package (see that package for further extensions)

## Data

- Updated the edge 'sign' attribute of `ison_marvel_relationships` to be a double (`-1`/`1`) to be compatible with the new `graph_balance()` and `{signnet}`

# migraph 0.8.0

## Classes
- Fixed coercion to `{igraph}` from data frames and updated read script
- Added `to_main_component()` to extract the main component of a network
- Added `to_onemode()` for moving to multimodal igraph objects
- Added `to_uniplex()` method to delete edge types and their edges from
  multiplex networks
- Added `to_simplex()` method to delete loops from a network
- Added `to_named()` method for randomly naming unlabeled networks

## Data
- Added `ison_mm`, `ison_mb`, `ison_bm`, and `ison_bb` projection illustration
  data
- Added `ison_karateka` community detection illustration data
- Added `ison_marvel_teams` and `ison_marvel_relationships` datasets
- Added `ison_m182` dataset of friends, social and task ties between 16
  anonymous students
- Renamed `adolescent_society` dataset to `ison_coleman`for consistency
- Data now listed at the bottom of the website References page

## Measures
- Added `graph_eigenvector()` for one mode networks
- Added `graph_balance()` for measuring structural balance
- Added `node_tie_census()`, `node_triad_census()`, `cluster_triad_census()`,
  and `graph_triad_census()`
- Separated out `graph_clustering()` into the cohesion measures
  `graph_density()`, `graph_reciprocity()`, `graph_transitivity()`,
  and `graph_equivalence()`
- Fixed `node_smallworld()` to use separated cohesion measures

## Models
- Added `blockmodel()` which masks its `{sna}` namesake but has the advantages
  of working with two-mode networks and retaining node names where available
  - Added `cluster_structural_equivalence()` and `cluster_regular_equivalence()`
    as bases for blockmodelling
  - Added `reduce_graph()` for creating a network from a blockmodel
- Added first vignette on structural holes, structural equivalence and regular
  equivalence blockmodelling

## Visualization
- Added `autographr()` for plotting graphs with sensible defaults
  - Uses a more contrastive discrete palette when some nodal attribute is given
  - Uses an alpha for edges, and edges will now be sized by edge weight, where
    available
  - Uses node labels, sans borders, where available
  - Uses different shaped nodes, and different fonts, for different node sets
  - Removed `ggraphlabel()` since core functionality now provided by autographr
- Added ability for `ggidentify()` to identify the node with the highest value
  of a specified node-level measure
- Added a couple of more specific visualization functions
  - Added `ggatyear()` for subsetting and plotting edgelists at year
  - Updated `gglineage()` to return a graph colored according to lineage
    - Added tick marks
- Added several more specific functions for diagnosing and visualising
  blockmodels
  - Added `ggtree()` for neatly visualising hierarchical clusters
  - Added `ggidentify_clusters()` for identifying which number of clusters
  is most appropriate following the elbow method
- Fixed bug related to `ggraph::theme_graph()` present in a few different
  visualisation functions

# migraph 0.7.2

## Data

* Added `brandes` dataset for teaching centrality measures
* Added `adolescent_society` dataset for teaching friendship paradox
* Added `read_edgelist()` for importing Excel-created edgelists directly

## Visualization

* Added `ggraphlabel()` for one-function (1F) plotting label-based network
  graphs
* Added `ggevolution()` for 1F-plotting begin/end graph comparisons
* Added `ggraphgrid()` for 1F snap-to-grid graph layouts based on
  Fruchterman-Reingold or Kamada-Kawai
* Added `ggidentify()` for 1F identifying nodes with maximum scores based on
  some arbitrary function

## Manipulation

* Added `to_undirected()` for symmetrising networks of all types
* Made existing `to_` functions S3 methods

# migraph 0.7.1

## Classes

- Fixed Unicode char bug in coercion documentation

# migraph 0.7.0

## Classes

- Closed #100 by converting `as_` coercion functions to S3 methods

    - Added a little more readable documentation
    - Fixed bug with `as_matrix()` weighting
    - Fixed bug with `as_tidygraph()`

## Visualisation

- Closed #92 by adding `gglineage()` for graphing a citation network through
  time
- Closed #99 by adding `ggevolution()` for graphing two timepoints of the same
  network side by side
- Closed #102 by adding `ggraphgrid()` for locking a graph to a grid
- Slight improvements to `plot.igraph()` defaults

## Analysis

- Added tidygraph lookups to `node_` functions

# migraph 0.6.6

## Classes

-   Fixed bug in `as_matrix()` with frame matrix by dropping (rarely
    necessary) functionality

    -   Improved handling of weights column in three-column edgelists
    -   Improved documentation of `as_` functions

## Visualisation

-   Fixed bugs in `plot_releases()` with more graceful handling of http
    errors

    -   Added online condition to example in documentation
    -   Specified encoding for more silent operation

# migraph 0.6.5

## Package

-   Removed unused package dependencies (`{R6}`, `{ggraph}`)
-   Avoided M1mac check issue by dropping sensitive `netlm()` test
-   Added some tests

## Classes

-   Renamed `binarise()` to `to_unweighted()`
-   Added `to_unnamed()` for unlabelling networks

# migraph 0.6.4

## Package

-   Extended R version dependence back to 3.6.\*

## Classes

-   Added `binarise()` for unweighting networks
-   Fixed bug in `as_tidygraph()` when passed a tbl_graph directly

## Visualization

-   Added `plot_releases()` for more general use
-   Fixed bug in `plot.igraph()` with layouts and one-mode graphs

# migraph 0.6.3

## Package

-   Updated README

    -   Updated installation instructions for CRAN
    -   Added package functions overview

-   Added `CITATION` details

## Classes

-   Separated coercion (previously conversion) and manipulation

-   Added some more inter-class coercion tests

-   Fixed bug in how `as_network()` sometimes coerced two-mode networks
    into much larger dimension matrices

-   Added more `is_` tests for class-independent property tests

    -   Added `is_weighted()`
    -   Added `is_directed()`
    -   Added `is_labelled()`

## Data

-   Added @csteglich 's `read_ucinet()` and `write_ucinet()` functions

    -   `read_ucinet()` offers a file-picker when file path unknown
    -   `read_ucinet()` now imports to an igraph-class object by
        default, with an argument to allow other alternatives
    -   `write_ucinet()` works with all migraph-compatible objects

-   Updated `mpn_bristol` documentation

-   Added `create_star()` function

    -   Added in-star/out-star option via `directed =` argument
    -   Updated `create_` documentation

-   Renamed `sample_affiliation()` to `generate_random()`

    -   Rewrote `generate_random()` to be able to generate random one-
        or two-mode networks
    -   Updated documentation

## Models

-   Added test for `print.blockmodel()`

# migraph 0.6.2

2021-04-13

## Package

-   Reran `usethis::use_mit_license("James Hollway")`. MIT License file
    now contains only the standard two lines.
-   Removed `\dontrun` from examples. `netlm()` now runs in \<5 seconds.
-   Fixed missing website item

# migraph 0.6.1

2021-04-11

## Package

-   Closed \#21 by elaborating DESCRIPTION file in preparation for CRAN
    submission
-   Updated several old URLs in documentation

## Classes

-   Closed \#85 by adding `as_network()` to coerce objects into network
    class
-   Modified other coercion functions to also work with network class
    objects

# migraph 0.6.0

2021-03-03

## Package

-   Moved package's Github repository from `jhollway/` to `snlab-ch/`
    organisation
-   Trimmed some package dependencies and added others

## Data

-   Elaborated documentation for the remainder of the datasets

    -   Now all datasets in this package are titled with whether they
        are one-mode, two-mode, or three-mode

## Measures

-   Fixed bug in `graph_degree()` where data was hard-coded in

## Models

-   Closed \#18 by adding `blockmodel_concor()` for employing the CONCOR
    algorithm to blockmodel both one-mode and two-mode networks

    -   Added a new print method for "blockmodel"-class objects based on
        the `print.blockmodel()` method in the `{sna}` package that also
        prints blockmodel results for two-mode networks consistently
    -   Added a new plot method for "blockmodel"-class objects that
        leverages `{ggplot2}` for pretty plotting and that better
        inherits names from the underlying object

# migraph 0.5.0

2021-02-06

## Package

-   Closed \#81 by making `{migraph}` depend on R versions 4.0 or above
-   Updated PR template

## Classes

-   Added functions for class conversion between migraph-consistent
    graph formats
-   `as_matrix()` function to coerce objects into an adjacency or
    incidence matrix class
-   `as_igraph()` function to coerce objects into an `{igraph}` graph
    class
-   `as_tidygraph()` function to coerce objects into an `{tidygraph}`
    tbl_graph class
-   Closed \#79 by adding `is_twomode()` function to check whether
    network is two-mode on all object types

## Data

-   Renamed several datasets and elaborated their documentation

    -   `mpn_mexicanpower` was renamed to `mpn_elite_mex`
    -   `mpn_powerelite` was renamed to `mpn_elite_usa_advice`
    -   `mpn_opensecrets` was renamed to `mpn_elite_usa_money`

-   Reconstructed several creation functions to take universal
    (one-mode/two-mode) input: specifying `n = 5` creates a one-mode
    network, while specifying `n = c(5, 5)` creates a two-mode network

    -   Added `create_empty()`
    -   Added `create_complete()`
    -   Closed \#65 by extending `create_ring()` to create rings of
        varying breadth
    -   Closed \#66 by extending `create_components()` (renamed from
        `create_silos()`) to create networks with varying numbers of
        components
    -   Added `sample_affiliation()` for random two-mode networks
    -   Removed `create_match()` and `create_nest()`

## Measures

-   Renamed `centrality_` functions with `node_` prefix and ensured they
    all also wrapped one-mode measures

    -   `centrality_degree()` renamed to `node_degree()`
    -   `centrality_closeness()` renamed to `node_closeness()`
    -   `centrality_betweenness()` renamed to `node_betweenness()`
    -   Closed \#31 by adding `node_eigenvector()`

-   Re-added `node_constraint()` for calculating Burt's constraint
    measure for one- and two-mode networks

-   Re-added `node_smallworld()` for calculating Watts-Strogatz measure
    of small-worldness for two-mode networks

-   Closed \#32 by re-adding centralization functions for one- and
    two-mode networks

    -   `graph_degree()` for degree centralization
    -   `graph_closeness()` for closeness centralization
    -   `graph_betweenness()` for betweenness centralization

-   Re-added `graph_clustering()` for calculating (see Knoke et al
    2021):

    -   transitivity on one-mode networks
    -   shared four-cycles on two-mode networks
    -   congruent four-cycles on three-mode networks

## Models

-   Re-added `netlm()` for performing linear regression for multimodal
    network data

    -   Closed \#76 by changing `netlm()` to accept a formula-based
        input
    -   Closed \#77 by adding `print.summary.netlm()` for `netlm()`
        regressions

## Visualization

-   Closed \#82 by re-adding a version `plot.igraph()` with sensible
    defaults for two-mode networks

# migraph 0.4.1

2021-01-11

## Package

-   pkgdown now deploys after release
-   Reexported a number of `{igraph}` and `{tidygraph}` functions for
    internal use
-   Completed some `convert_` and `project_` documentation

## Data

-   Updated mpn\_ data source references

## Analysis

-   Added centrality measures that take (and if necessary return)
    matrix, igraph, or tidygraph objects, and offer a correct
    normalization for two-mode networks

    -   Added `centrality_degree()`
    -   Added `centrality_closeness()`
    -   Added `centrality_betweenness()`

# migraph 0.4.0

2021-01-08

## Package

-   Package name change from roctopus to `{migraph}`

    -   Closed \#50 with new logo

-   Now builds Linux binary too

## Manipulation

-   Added `project_rows()` and `project_cols()` to make it easier to
    project two-mode networks in different formats (matrix, igraph,
    tidygraph) into projected versions in the same format
-   Closed \#30 with conversion from different data frame formats, e.g.
    weighted and unweighted edgelists, into an incidence matrix with
    `as_incidence_matrix()`

## Data

-   Renamed data related to the book "Multimodal Political Networks"
    with "mpn\_" prefix

# roctopus 0.3.0

2020-11-06

## Data

-   Data creation updated for `{tidygraph}` defaults

    -   Renamed `create_lattice()` to `create_chain()` to avoid
        conflicts with `tidygraph::create_lattice()`
    -   Renamed `create_poles()` to `create_silos()`
    -   Renamed `create_random()` to `play_twomode()` to avoid conflicts
        with `tidygraph::play_bipartite()`
    -   Added export options for all `create_` and `play_` functions as
        `tbl_graph` (default), `igraph`, and base matrix
    -   Updated tests for new `create_` and `play_` function names

-   Packaged data updated for `{tidygraph}` defaults

    -   Renamed packaged data from book to `mpn_`

# roctopus 0.2.6

2020-08-18

## Analysis

-   Renamed `twomode_netlm()` to `netlm2()`
-   Fixed various printing issues with `netlm2()`

## Package

-   Added tests for `netlm2()`
-   Added tests for `create_random()`

## Data

-   Added `opensecrets` data
-   Added `powerelite` data

# roctopus 0.2.5

2020-07-21

## Package

-   Renamed data documentation scripts
-   Added tests for most `create_()` functions
-   Renamed test scripts and removed `context()` declaration
-   `create_nest()` now exports matrix object

# roctopus 0.2.4

2020-07-20

## Package

-   Added `row_project()` and `col_project()` functions to make it
    easier to remember project directions

# roctopus 0.2.3

2020-07-19

## Data

-   Added `mexicanpower` dataset
-   Added `senate112` datasets
-   Updated `evs` datasets to matrices

## Package

-   Added `df_to_mat()` function for converting regular adjacency and
    incidence data frames into matrices

# roctopus 0.2.1

2020-07-17

## Data

-   Added `ryanair` dataset from Christopoulos 2006

## Package

-   Split workflow into pull-request and merge/push prompted actions
-   Added codecov calculation to workflows

# roctopus 0.2.0

2020-07-03

## Data

-   Added `evs` datasets from Knoke et al 2020

# roctopus 0.1.0

2020-06-30

## Data

-   Added `bristol` dataset from Knoke et al 2020

# roctopus 0.0.4

2018-12-20

## Package

-   Added hex sticker
-   Updated README with more detailed installation information
-   Pkgdown exports to <https://jhollway.bitbucket.io/roctopus/>

## Analysis

-   Added `twomode_modularity()` to calculate modularity in two-mode
    networks

## Visualization

-   Added `plot_multilevel()` that rotates a force-directed `igraph`
    plot in three dimensions to reveal multilevel structure
-   Added `plot_globalnet()` to map a multilevel network on to a
    javascript, rotatable global

# roctopus 0.0.3

2018-08-25

## Analysis

-   Added `twomode_smallworld()` to calculate observed/expected
    clustering, observed/expected path-length, and the observed/expected
    clustering ratio
-   Added `twomode_2x2()` to identify dominance and coherence values for
    networks over time
-   Updated `twomode_coherence()` to allow for introduction of
    second-mode attributes
-   Renamed `twomode_fragmentation()` to `twomode_components()`

## Visualisation

-   Added `plot_2x2()` to plot values through a two-by-two matrix

# roctopus 0.0.2

2018-08-14

## Package

-   Renamed package to `roctopus`
-   Added two-mode @family tag to documentation

## Analysis

-   Added `twomode_fragmentation()` to calculate number of components in
    two-mode networks and identify their membership
-   Added `twomode_dominance()` to allow an nodal attribute to be passed
    to the function to weight the centralization measure
-   Added `twomode_coherence()` to calculate Jaccard similarity

## Visualisation

-   Added `plot_twomode()`, which wraps
    `plot(igraph::graph_from_incidence_matrix())` with some useful
    defaults

    -   coloured grayscale by default, with green/blue option
    -   shaped circles and squares by default

# roctopus 0.0.1

2018-07-30

## Package

-   Initialised package
-   Added `README.md` file with instructions on how to install package
-   Added `LICENSE` file and pointed to bug/issue tracker
-   Added a `NEWS.md` file to track changes to the package

## Analysis

-   Added `twomode_clustering()` to calculate percentage of three-paths
    closed by four-paths
-   Added `twomode_lattice()` to create two-mode lattices
-   Added `twomode_centralization_degree()` to calculate degree
    centralization in two-mode networks, for rows, columns, or both
-   Added `twomode_centralization_between()` to calculate betweenness
    centralization in two-mode networks
-   Added `twomode_constraint()` to calculate network constraint in
    two-mode networks
-   Added `arrange.vars()` to rearrange variables by position
