# migraph 1.1.5

2023-11-02

## Members

- Fixed documentation issue with `summary.node_member()`

## Motifs

- Fixed documentation issue with `node_census()`

# migraph 1.1.4

2023-11-01

## Marks

- `print.node_mark()` and `print.tie_mark()` now allow infinite 'n' width

## Measures

- `print.node_measure()` and `print.tie_measure()` now allow infinite 'n' width
  - Digit rounding is now fixed in print and summary methods

## Members

- `print.node_member()` now prints the vector
  - Previous functionality is now available in `summary.node_member()`

# migraph 1.1.3

2023-10-25

## Tutorials

- Fixed bug in network setup in 'tutorial4' (Community)
- Fixed bug in blockmodelling interpretation in 'tutorial5' (Position)
- Added more to core-periphery section to 'tutorial6' (Topology)
- Added network resilience section to 'tutorial6' (Topology)

## Measures

- Added `network_richclub()` to return a network's rich-club coefficient (closed #223)
- `node_core()` now offers eigenvector centrality-based rank selection in addition to degree-centrality
  - This can be selected with the argument `method = "eigenvector"`
- Added Krackhardt's graph theoretic dimensions of hierarchy measures, including
  - `network_connectedness()`
  - `network_efficiency()`
  - `network_upperbound()`
- Added `node_eccentricity()`, which wraps `igraph::eccentricity()`
- Added `node_neighbours_degree()`, which wraps `igraph::knn()`
- Added `tie_cohesion()` to measure how embedded ties are
  
## Members

- Added `node_louvain()` community detection algorithm
- Added `node_leiden()` community detection algorithm
  - Note that this function optimises the Constant Potts Model rather than modularity

# migraph 1.1.2

2023-10-18

## Tutorials

- Added more code annotations in 'tutorial4' (Community)
- Elaborated 'tutorial5' (was named Equivalence, now Position)
- Chunks in tutorials that are incremental now hidden upon extraction using `purl = FALSE` argument

## Members

- Improved printing of node_members and node_measures objects

# migraph 1.1.1

2023-10-11

## Package

- Added more questions to the Centrality tutorial (3)
- Added more instruction and more questions to the Community tutorial (4)

## Measures

- Fixed scale, labelling, and other issues in `plot.node_measure()`

## Members

- Added `node_optimal()`, `node_infomap()`, `node_spinglass()`, and `node_leading_eigen()` to extend the community detection options available in the package

# migraph 1.1.0

## Package

- Added Macports option to the README (closes #274, thank you @barracuda156)
- Updates to the Centrality tutorial

## Measures

- Added `node_outdegree()`, `node_indegree()`, `network_outdegee()`, and `network_indegree()` wrappers
- Added `network_reach()`
- Added `node_harmonic()` and `network_harmonic()`
- Added `node_pagerank()`
- `plot.node_measure()` now returns a single plot for one-mode networks with a frequency histogram and a density overlay

## Data

- Upgraded all old igraph data to work with most recent version

# migraph 1.0.1

## Measures

- Fixed backends in `node_closeness()` and `node_betweenness()` in preparation for an `{igraph}` deprecation (merci @maelle)

## Models

- Fixed #272 by making `test_random()` and `test_permutation()` rely on graph dimensions rather than graph order (thank you finding this @rabenton)

## Tutorials

- Fixed some defunct function names

# migraph 1.0.0

## Package

- Breaking changes by moving the making, manipulating, and mapping functions to `{manynet}`
  - All `create_*()` and `generate_*()` functions are now in `{manynet}`
  - All `read_*()` and `write_*()` functions are now in `{manynet}`
  - All `as_*()` and `to_*()` functions are now in `{manynet}`
  - Most `is_*()` functions are now in `{manynet}`
  - All `add_*()` and `join_*()` functions are now in `{manynet}`
  - Functions that grab metadata from network objects, 
  e.g. `network_nodes()`, `node_names()`, and `tie_weights()`, are now in `{manynet}`
  - All `autograph*()` functions are now in `{manynet}`
  - `{manynet}` is now a Depends, but many dependencies removed or moved to `{manynet}`
- Most functions now expect `.data` as their first argument; previously it was `object`
- Moved data vignette (now a tutorial) and visualisation tutorial to `{manynet}`
- Updated README and pkgdown structure accordingly

## Measures

- Added `over_time()` and `over_waves()` to measure (potentially parallelised) over split graphs
  - Added class construction and a plot method for resulting `network_measures` object
- Each of the four main types of centrality now get their own page of documentation
- Added `node_alpha()` for calculating alpha centrality, mostly just a wrapper for `igraph::alpha_centrality()`
- Added `node_power()` for calculating beta or Bonacich centrality, mostly just a wrapper for `igraph::power_centrality()`, though also correctly accounts for two-mode networks
- Renamed `node_homophily()` to `node_heterophily()`, which is more accurate and in line with the scale's direction
  - `node_heterophily()` now calculates EI indices in a faster, vectorised form, 
  instead of the older, slower solution that calculated `network_homophily()` over all ego networks
- Renamed `network_homophily()` to `network_heterophily()`
  - `network_heterophily()` now ignores missing values
- `network_congruency()` is now more explicit about its data expectations

## Models

- `k_strict()`, `k_elbow()` and `k_silhouette()` are now exported and documented
- Added `play_segregation()` for playing Schelling segregation models with various parameters
- Fixed some `diff_model` and `diffs_model` plotting irregularities
- Fixed bug in how merges were constructed as a distance matrix in `cluster_concor()`

## Data

- Moved `ison_` data to `{manynet}`

# migraph 0.13.2

## Package

- Updated README with better structuring and explanation
  - Also improved figure quality

## Makes

- Added `read_matrix()` for importing CSVs of adjacency/incidence matrices
- Added `read_dynetml()` for importing DynetML .xml files (closed #261)

## Manipulations

- Added list method for `as_tidygraph()` for merging nodelists and edgelists
  - Note that a named list is expected, with names "nodes" and "ties"
- Added igraph method for `as_siena()`
  - Note that this is a WIP proof of concept, and will currently only create the DV

# migraph 0.13.1

## Package

- Added hints, solutions, comments, and questions to equivalence and centrality tutorials
- Changed theme across all tutorials

## Makes

- Fixed bug in `read_pajek()` where multiple networks/ties were causing an issue for partition assignment

## Models

- Fixed bug in `play_diffusion()` relating to latency inversion

# migraph 0.13.0

## Package

- Added hints and questions to community tutorial
- Added diffusion tutorial

## Marks

- Added `is_aperiodic()` for testing whether a network is aperiodic 
(the greatest common divisor for all cycles in the network is 1)
  - Note that for computational efficiency, this will check only up to a specified
  path length (by default 4)
- `node_is_max()` and `node_is_min()` now take a "rank" argument for selecting more
than the first ranked maxima or minima

## Manipulations

- Added several new functions for splitting networks into a list of networks
  - Added `to_components()` to return the components of a network as a list of networks
  - Added `to_egos()` to return the ego networks of a network as a list of networks
  - Added `to_subgraphs()` to return attribute-based subgraphs as a list of networks
  
## Measures

- Added `network_richness()` and `node_richness()` for calculating the richness 
(a common diversity measure) of an attribute in a network

## Models

- Extended `play_diffusion()` to include more compartment and transition options
  - The print method now tabulates the compartment sums per step
  - The summary method now presents the diffusion event list
  - The plot method now:
    - plots lines for S and I compartments, as blue and red lines respectively, 
    if available or informs the user if no diffusion could be simulated
    - plots lines for E and R compartments, as orange and green lines respectively, if relevant
    - plots a bar graph behind showing the number of new infections per step
  - Added "transmissibility" parameter to allow for more probabilistic contagion,
  by default 1 (all contacts over the threshold result in contagion)
  - Added "latency" parameter to allow for an Exposed compartment to be included,
  by default 0 (no incubation period)
  - Added "recovery" rate parameter to allow for a Recovered compartment to be included,
  by default 0 (no recovery)
  - Added "waning" parameter to allow for returns to the Susceptible compartment,
  by default 0 (any recovered have lifelong immunity)
  - Added "immune" parameter to allow for issuing of targetted vaccinations or related
  - "thresholds" can now be proportions, in which case they are interpreted as complex
  - "seeds" is now 1 by default
- Added `play_diffusions()` for running a diffusion model multiple times
  - Note for accelerating documentation and tutorials, this is 5 by default,
  but for publication quality results this should be increased
  - A print method tabulates the compartment sums per step per simulation
  - A summary method tabulates the steps until complete infection (or recovery)
  for each simulation
  - A plot method visualises the loess for each compartment across all simulations
  - Note that this function uses `{furrr}` and so a multicore or multisession strategy
  can be used for parallelisation (but this only makes sense for many simulations)
- Added `play_learning()` for running a DeGroot learning model
  - A print method tabulates nodes beliefs at each step
  - A summary method informs how many steps it took until convergence or
  whether there was no convergence after _t_ steps
  - A plot method visualises the belief trajectories for each node
- `network_reg()` now declares the reference category for nominal variables
  
## Mapping

- Added `autographs()` for autoplotting lists of networks

# migraph 0.12.6

## Package

- Changed tutorial naming structure to numeric
- Made all existing tutorials solution oriented
- Converted visualisation vignette into tutorial
  - Added further instruction as to how to change e.g. node_color colors
- Converted centrality vignette into tutorial
- Converted regression vignette into tutorial
  - Added a lot more interactivity to regression tutorial
- Deleted vignette instructions off of the README
- Added some core/coreness aspects to topology tutorial

## Manipulations

- `mutate()` now works with igraph objects

## Marks

- Added `node_is_random()` for selecting n nodes at random

## Models

- Added first draft of (SI) `play_diffusion()` model
  - Added diff_model class, together with print, summary, and plot methods

# migraph 0.12.5

## Package

- Added topology tutorial

## Makes

- Fixed integer recognition bug in `generate_random()`

## Manipulating

- Added more similarity options for projection (`to_mode1()` and `to_mode2()`)
- Fixed bug in `to_redirected.igraph()` where routing through an edgelist caused problems

## Measures

- Elaborated documentation on modularity
- Added `network_scalefree()` for returning power law alpha/exponent
  - A message is given if the KS p-value is less than 0.05
  
## Data

- Added `ison_lotr` dataset for examples using interactions among Lord of the Rings
  characters in the books

# migraph 0.12.4

## Package

- Converted 'visualisation' vignette to a learnr tutorial
- Deleted troublesome URLs to correlatesofwar.org
- Brokerage census gets their own documentation page

# migraph 0.12.3

## Package

- Converted 'community' vignette to a learnr tutorial
- Converted 'equivalence' vignette to a learnr tutorial

## Motifs

- Added node names to node_motif class where available
- Added `node_brokerage_census()` and `network_brokerage_census()` for counting Gould-Fernandez brokerage roles

# migraph 0.12.2

## Package

- Fixed several typos in the centrality vignette and reexported figures
- Added community detection vignette

## Make

- `create_lattice()` now conforms to other `create_*()` functions in how it interprets `"n"`
  - from an inferred `"n"` for a one-mode network, it will create a transitive lattice of as even dimensions as possible
  - for a two-mode network, this depends on how balanced the two modes are, and is still a work in progress... (WIP)

## Marks

- Added `is_eulerian()` for a logical expression of whether the network has an Eulerian path

## Measures

- `network_smallworld()` now takes a method argument for different ways of calculating a small-world coefficient
  - "omega" (the new default) offers a better range, 0 to 1, than the previous (now "sigma") metric.
  - "SWI" is also included and offers a 0 to 1 range, but where 1 may not be realisable
- Added `node_diversity()` for calculating heterogeneity among each nodes' ego network
- Added `node_homophily()` for calculating homophilous ties among each nodes' ego network
- Added `node_reciprocity()` for calculating each node's reciprocity
- Added `node_transitivity()` for calculating each node's transitivity/clustering

## Memberships

- Added wrappers for several community detection algorithms from igraph,
  unlike tidygraph these can operate on objects directly
  - Added `node_walktrap()`
  - Added `node_edge_betweenness()`
  - Added `node_fast_greedy()`

## Mapping

- Reversed blue/red colour assignment for binary variables

# migraph 0.12.1

## Manipulations

- `to_twomode()` now returns an undirected network
- Added `to_anti()` for obtaining the complement of the given network
  - Note that unlike the implementation in `{igraph}`, this respects two-mode constructions

## Marks

- Added `is_perfect_matching()` for a logical expression of whether the maximum matching of a network is also perfect
- Added `node_is_core()` for a logical vector of which nodes are members of the core

## Measures

- `node_degree()` now has an additional parameter for trading off between degree and strength in the case of weighted networks
- Added `node_power()` for Bonacich power centrality for both one- and two-mode networks (closed #193)
- Tie centrality measures now have their own help page (WIP)

## Mapping

- Fixed `autographr()` tests to work with new version of `{ggraph}` (closed #247, thanks @henriquesposito)

# migraph 0.12.0

## Package

- `graph_*()` functions now renamed to `network_*()` for terminological consistency
- Added `p2visualization` vignette
  - more motivation re Tufte and Brandes et al
  - an overview on key multimodal layouts
  - a few demonstrations of `{patchwork}` functionality

# migraph 0.11.4

## Package

- Added igraph/migraph layout comparison to README

# migraph 0.11.3

## Package

-   Fixed CRAN LaTeX issue relating to underscores in manual figures

## Mapping

-   Fixed bug in plotting space coordinates for named two-mode networks
-   Suppressed messages relating to 'graph' class being defined by multiple packages

# migraph 0.11.2

## Package

-   Filled in some further documentation
-   Dropped some older defunct functions

## Measures

-   Renamed `graph_blau_index()` to `graph_diversity()`
-   Renamed `graph_ei_index()` to `graph_homophily()`

## Mapping

-   Fixed bug with `autographr()`'s "node_group" argument

# migraph 0.11.1

## Package

-   Fixed documentation issues (URLs, equations)

# migraph 0.11.0

## Package

-   Reference and articles tabs on package website now called "Function Overview" and "Practical Lessons" respectively
-   Reinstated equivalence and blockmodelling vignettes
-   All vignettes now precompiled to avoid CRAN issues

## Manipulation

-   Added `as_graphAM()` methods for all migraph-consistent object classes so `{Rgraphviz}` can be used effectively
-   Added `as_igraph()`, `as_tidygraph()`, and `as_network()` methods for `{RSiena}` sienaData objects (thanks @JaelTan, closed #94)
-   Added `as_edgelist()` and `as_matrix()` methods for `network.goldfish` class objects
-   The `"twomode"` argument in `as_matrix()` is now `NULL` by default, allowing both one-mode and two-mode coercion
-   `to_mode1()` and `to_mode2()` now take an extra argument to produce weighted projections by different "similarity" measures
    -   "count" (the default) returns a raw count of the coincidence of nodes in the specified mode with nodes of the other mode
    -   "jaccard" (Jaccard index) offers a measure of opportunity weighted by participation
    -   "rand" (Simple Matching Coefficient) offers a measure of behavioral mirroring
    -   "pearson" (Pearson's correlation) and "yule" (Yule's Q) offer correlation coefficients for valued and binary data, respectively
    -   These options work for edgelists, matrices, igraph and tidygraph, and network objects
-   Added `to_matching()` methods to transform a two-mode network or network with some other (binary) "mark" attribute into a network of matching ties
-   Renamed `to_main_component()` to `to_giant()` to be more space efficient
    -   Added methods for edgelists and matrices
-   Added two-mode application for `to_blocks()` (closed #242)
    -   Fixed bug in `to_blocks()` where NA blocks couldn't be subsequently coerced
-   Filled in a number of S3 methods
    -   `to_blocks()`, `to_subgraph()`, and `to_ties()` are now S3 methods, returning objects of the same class as given
    -   Added `to_onemode()` method for matrices
    -   Added `to_twomode()` methods for igraph, tidygraph, and network
    -   Added network methods for `to_named()`, `to_redirected()`, `to_uniplex()`, and `to_unsigned()`
    -   Added edgelist methods for `to_undirected()` and `to_uniplex()`
    -   Added matrix method for `to_uniplex()`
-   Fixed bug where `to_unweighted()` didn't respect the "threshold" specified

## Marks

-   Fixed eternal loop bug where `node_mode()` couldn't produce a "mark" class object
-   Fixed bug where node names/labels were not being added to mark objects
-   Fixed bug in `is_twomode()` where labelling information was being ignored

## Measures

-   `graph_core()` now runs `node_core()` (see below) if no "membership" vector is provided

## Membership

-   Added `node_core()` for partitioning nodes into core and periphery memberships
-   Fixed floating point bug in `k_strict()`

## Models

-   Added "tertius" effect for `network_reg()`
-   "ego" and "alter" effects now work better for two-mode networks in `network_reg()`

## Mapping

-   Added "hierarchy" and "alluvial" layout methods
-   Added "railway" and "ladder" layout methods (closed #125)
-   Added "concentric" layout method
-   Restructured `autographr()` to improve future debugging and development
-   `autographr()` now rotates labels for partitioning layouts, including "concentric", so that they are readable and overlap less
-   Fixed `gglineage()` to use "alluvial" layout and better position nodes on the *x*-axis

## Data

-   `mpn_cow_igo` now includes "polity2" scores
-   Added visualisations to some `ison_` and `mpn_` data documentation (closed #237)

# migraph 0.10.6

## Package

-   Fixed URL error in a vignette

# migraph 0.10.5

## Package

-   Dropped vignettes for now to ensure package makes it on to CRAN

# migraph 0.10.4

## Package

-   Fixed some URL issues for CRAN

## Manipulation

-   Split `to_*()` functions into reformatting (changing properties) and transforming (changing dimensions) documentation

# migraph 0.10.3

## Package

-   Updated the DESCRIPTION and CITATION
-   Renamed `edge_*()` to `tie_*()` to offer more (SNA) consistent vocabulary
-   Added DOIs to as much data and documentation as possible (closed #236, thanks @JaelTan)
-   Some further rationalisation of the documentation
-   Dropped visualization vignette for now

## Manipulation

-   Added methods for converting `network.goldfish` objects (and linked events and nodelists) to migraph-compatible objects (closed #96)
-   Renamed `add_node_attributes()` to `add_node_attribute()` and `add_edge_attributes()` to `add_tie_attribute()`

## Marks

-   All `is_*()` are now considered graph-level 'marks'
-   Added 'node_mark' and 'tie_mark' classes with printing methods (closed #233)
-   Added `node_is_isolate()` for marking isolates
-   Added `node_is_max()`, `node_is_min()`, `tie_is_max()`, `tie_is_min()` for converting 'measures' into 'marks'

## Measures

-   Printing 'node_measure' class objects now is prettier, extending the width of the console, indicating how many additional observations, and separates out each mode (closed #232)

## Motifs

-   Added print method for `graph_motif` (fixed #234)

## Memberships

-   Equivalence examples now `\dontrun`

## Models

-   `cluster_concor()` and `cluster_hierarchical()` are now exported

## Mapping

-   `autographr()` no longer requires "highlight_measure" and "identify_function" arguments as users can now convert 'measures' to 'marks' and use these for "node_color" or "edge_color"

## Data

-   Added prints of each data object to `@format` for more consistent documentation
-   Added `ison_brandes2` dataset, a two-mode version of the original one-mode dataset
-   Added `mpn_cow_trade` and `mpn_cow_igo` datasets (thanks @JaelTan)
-   Fixed non-unique names in `mpn_elite_mex`

# migraph 0.10.2

## Memberships

-   Further shortened equivalence examples

# migraph 0.10.1

## Measures

-   Added `node_reach()` for calculating reach centrality (closed #196)
-   Separated (again) centrality and centralisation documentation

## Memberships

-   Shortened equivalence examples

# migraph 0.10.0

## Package

-   Reduced package dependencies by 5
    -   `{concaveman}`, `{ggdendro}`, `{oaqc}` are now Suggests; the user is prompted to install them when required in `autographr()`, `plot.member()`, and `node_quad_census()` respectively
    -   `{stringr}` and `{tibble}` replaced with base or other code and therefore no longer necessary
-   Relabelled scripts to follow website function structure
    -   Added `@family` tags for improved cross-referencing
    -   Added a lot more references/sources
-   README elaborated, including listing functions and data in the package
-   Switched to S3 classes as outputs for most functions
    -   Several methods, e.g. `print()`, `plot()`, `summary()`, have been added for them (see appropriate sections below)

## Making

-   All `create_` and `generate_` functions now:
    -   work when `n` passed an existing network
    -   work with two-mode networks, including `create_tree()`, `generate_smallworld()`, `generate_scalefree()`
    -   return undirected network by default
-   Some `create_` functions can now take a membership vector or split into equal partitions by default
    -   `create_components()` no longer accepts a number of components, instead relying on the membership vector
    -   Added `create_core()` for creating core-periphery graphs
-   `generate_random()` now inherits attributes from any network

## Manipulation

-   Added a couple of `to_` functions useful for working with networks of different types
    -   Added `to_redirected()` for adding or swapping direction to networks (closed #219)
    -   Added `to_blocks()` for reducing a network down by a membership vector; `blockmodel()` and `reduce_graph()` are now deprecated
    -   `to_multilevel.igraph()` now only works on two-mode networks; returns the original network if passed a one-mode network
-   Fixed some bugs in a number of `is_` functions
    -   `is_signed.data.frame()` and `is_signed.matrix()` now rely on new helper `is.wholenumber()` rather than misleading `is.integer()`
    -   `is_directed.igraph()` and `is_directed.matrix()` now return FALSE for two-mode networks
    -   `is_connected()` now returns result for strong components if directed and weak components if undirected
-   `as_igraph.data.frame()` now infers third column as weight

## Marks

-   Added new set of functions that return logical vectors for nodes and edges:
    -   `edge_multiple()`, `edge_loop()`, `edge_reciprocal()` moved from measures
    -   Added `edge_bridges()`

## Measures

-   A new `"edge_measure"` S3 class has been added, along with `print()` and `plot()` methods
-   Added `summary.node_measure()` method for printing a summary by a membership vector; `summarise_statistics()` is now deprecated
-   All cohesion, connection, and diversity measures now return `"graph_measure"` class results
    -   `graph_components()` now calculates strong components for directed networks else weak components
    -   `print.graph_measure()` now correctly labels two-mode results where a vector is given
-   Added new script for measuring features, including `graph_smallworld()`
    -   Added `graph_core()` for calculating correlation of an observed network to a core-periphery network of the same dimensions (closed #39)
    -   Added `graph_factions()` for calculating correlation of an observed network to a component network of the same dimensions (closed #40)
    -   Added `graph_modularity()` for calculating modularity of an observed network, including modularity for two-mode networks (closed #144)
-   Added new script for measuring structural holes, including `node_constraint()`
    -   Added several additional measures of structural holes: `node_bridges()`, `node_redundancy()`, `node_effsize()`, `node_efficiency()`, `node_hierarchy()`
-   `node_betweenness()` no longer needs `nobigint` argument; just uses default from `{igraph}`

## Motifs

-   Added `"node_motif"` S3 class for the output of `node_*_census()` functions
    -   Added `print.node_motif()` for tibble-printing of census results
    -   Added `summary.node_motif()` to summarise censuses by a membership vector, replacing `group_tie_census()` and `group_triad_census()`, which are now deprecated
-   Added `"graph_motif"` S3 class for the output of `graph_*_census()` functions
-   Added `node_path_census()` for returning the shortest distances from each node to every other node (closed #222)
-   `node_tie_census()` now creates unique column names

## Memberships

-   Added new `"node_member"` S3 class for vectors of nodes' cluster memberships
    -   The class hides a hierarchical clustering object as an attribute, so `plot.node_member()` replaces `ggtree()`
-   Moved to an equivalence identification scheme that hides many of the technical aspects from users when unnecessary
    -   Added `node_equivalence()` for identifying nodes' membership in classes equivalent with respect to some arbitrary motif census
        -   `"hierarchical"` and `"concor"` now options for `cluster` within `node_equivalence()`; `blockmodel_concor()` is now deprecated (closed #123)
        -   `"elbow"` now an option for `k` selection within `node_equivalence()`; `ggidentify_clusters()` is now deprecated
        -   Added `"silhouette"` and `"strict"` options for `k` selection (closed #197)
        -   Added option for `k` to be defined
        -   There is now an argument `distance` passed to `stats::dist` that defines the distance metric (closed #36)
        -   The argument `range` constrains the number of `k` evaluated by `"elbow"` and `"silhouette"` to improve parsimony and avoid long elapsed times
    -   Added `node_automorphic_equivalence()` for identifying nodes' membership in automorphically-equivalent classes (closed #187)
    -   `node_structural_equivalence()` replaces `cluster_structural_equivalence()`
    -   `node_regular_equivalence()` replaces `cluster_regular_equivalence()`
-   Added community identification scheme that mirrors equivalence identification in many respects
    -   Added `node_kernaghinlin()` for identifying nodes' membership in communities based on the Kernaghin-Lin algorithm (thank you, @jaeltan, closed #198)
-   Added connected identification scheme that mirrors equivalence identification in many respects
    -   Added `node_coreness()` for nodes' *k*-core score (closed #200)
    -   Added `node_strong_components()` and `node_weak_components()` for more direct calls; `node_components()` now calculates strong components for directed networks else weak components

## Models

-   A single `"graph_test"` S3 class replaces `"cug_test"` and `"qap_test"`
    -   `plot.graph_test()` replaces `plot.cug_test()` and `plot.qap_test()`
    -   Added `print.graph_test()` method
-   `plot.matrix()` now plots adjacency/incidence matrices with sorting and horizontal/vertical lines if a membership vector is provided, effectively replacing `plot.block_model()`

## Mapping

-   `autographr()` can highlight nodes that max (by default) some measure (thank you, @BBieri, closed #224)
-   Added `layout_tbl_graph_stressgrid()` as an extra option
-   `ggatyear()` is deprecated

## Data

-   `ison_algebra`'s edge attributes now named "friends", "social", and "tasks"

# migraph 0.9.3

## Package

-   Trialling `{roxytest}`
-   Updated favicons
-   Updated several vignettes
    -   Closed #154 by building out data vignette
    -   Updated centrality vignette with more modern plotting
-   Added some more informative documentation families

## Making

-   Folded `m` argument into `p` for `generate_random()`, `p` can now be passed an integer to indicate the number of ties the network should have

## Manipulation

-   Refactored `to_edges()` to be \~26 times faster on average
-   Corrected edge labelling in `to_edges()`
-   Using `to_subgraph()` now instead of `dplyr::filter()` or `strain()`

## Mapping

-   Layouts now use `times` argument instead of `maxiter`

## Measures

-   Renamed `"measure"` class `"node_measure"` and added `"graph_measure"` class with print method
-   Overhaul of centrality measures
    -   Centrality and centralization measures now return normalized scores by default, `normalized` is now the second argument
    -   `directed` and `weights` arguments have been removed and are now imputed, if this is undesired please use `to_*()` first
    -   `node_degree()` now calculates strength centrality if network is weighted
    -   `node_eigenvector()` and `graph_eigenvector()` both work with two-mode networks
    -   Added `edge_degree()` and `edge_eigenvector()`, which both just apply the corresponding nodal measure to the edge graph
-   `edge_mutual()` renamed to `edge_reciprocal()`
-   Closed #225 by adding `graph_assortativity()`

## Modelling

-   Closed #151 with blockmodel coloring for signed graphs

## Data

-   Dropped weight from `mpn_elite_mex`
-   Dropped direction from `ison_brandes`

# migraph 0.9.2

## Package

-   Streamlined some examples to reduce testing time
-   Fixed a DOI URL for Ortmann and Brandes reference

# migraph 0.9.1

## Package

-   Streamlined some tests to reduce testing time

## Manipulation

-   `is_multiplex.igraph()` and `is_multiplex.tbl_graph()` now checks for multiple edge attributes
-   Added `strain()` as wrapper for `{dplyr}`'s `filter()`, renamed to avoid conflicts with `{stats}`

## Data

-   `ison_algebra` now unlabelled

# migraph 0.9.0

## Package

-   Recognised contributors Henrique Sposito and Jael Tan
-   Updated dependencies
    -   `{readxl}` is now *suggested*, but required if importing from an Excel sheet
    -   `{patchwork}` replaces `{gridExtra}` to make for more concise multiplot visualisations
    -   `{dplyr}` also serves to export `{magrittr}`'s pipe
    -   `{RColorBrewer}` has been dropped and the `Dark2` discrete set of colors is now internal
-   README has been updated and now compiles from a .Rmd file
-   Changed website theme to 'superhero'
-   All prior deprecated functions have been removed
-   Increased testing to \~80% (closed #126, #212)
-   CITATION has been updated too

## Making

-   Moved to @describeIn documentation (closed #215)
-   Distinguished `directed` and `direction` arguments in some functions; whereas `directed` is always logical (TRUE/FALSE), `direction` expects a character string, e.g. "in", "out", or "undirected"
-   `generate_permutation()` now has an additional logical argument, `with_attr`, that indicates whether any attributes from the original data should be passed to the permuted object
-   All `create_*()` functions now accept existing objects as their first argument and will create networks with the same dimensions
-   `read_pajek()` now imports nodal attributes alongside the main edges
-   `read_ucinet()` now enjoys clearer documentation

## Manipulation

-   All `as_*()` functions now retain weights where present; if you want an unweighted result, use `is_unweighted()` afterwards
    -   `as_edgelist.network()` now better handles edge weights
    -   `as_matrix.igraph()` now better handles edge signs
-   Pivoted to S3 methods for most manipulation functions for better dispatching and performance
    -   Added matrix, data.frame, network, igraph, and tbl_graph methods for `is_twomode()`, `is_directed()`, `is_weighted()`, `is_labelled()`, `is_signed()`, `is_multiplex()`, `is_complex()`, and `is_graph()`
    -   Added data.frame methods for `as_edgelist()`, and `to_unweighted()`, and improved the data.frame method for `as_matrix()`
    -   Added data.frame and matrix methods for `to_named()` and `to_unsigned()`
-   Added `to_edges()` for creating adjacency matrices using a network's edges as nodes
-   Renamed `project_rows()` and `project_cols()` functions to `to_mode1()` and `to_mode2()`, which is both more consistent with other functions naming conventions and more generic by avoiding the matrix-based row/column distinction
-   Added `node_mode()`, which returns a vector of the mode assignments of the nodes in a network
-   Added `edge_signs()`, which returns a vector of the sign assignments of the edges in a network

## Mapping

-   Added 'visualization' vignette that starts to introduce how `autographr()` works and how `{ggraph}` extends this
-   `autographr()` now incorporates `ggidentify()` functionality (closed #150)
-   `{patchwork}` is now used to assemble multiple plots together
-   Fixed #204 layout issues with `ggatyear()`

## Measures

-   Added new `measure` class and directed most `node_*()` functions to create objects of this class
    -   A print method for this class prints an abbreviated vector (the full vector is always still contained within the object) and prints elements from both modes in the event that the original object was two-mode (closed #202)
    -   A plot method replaces `ggdistrib()` and offers "hist" and "dens" methods for histograms and density plots respectively
-   Added some edge-based centrality measures (closed #165)
    -   `edge_betweenness()` wraps `{igraph}`'s function of the same name
    -   `edge_closeness()` measures the closeness centrality of nodes in an edge adjacency
-   Added several more measures of connectedness
    -   `node_cuts()` identifies articulation points (nodes) in a network
    -   `edge_bridges()` identifies edges that serve as bridges in a network
    -   `graph_cohesion()` measures how many nodes would need to be removed to increase the number of components (closed #192)
    -   `graph_adhesion()` measures how many edges would need to be removed to increase the number of components
    -   `graph_length()` measures the average path length
    -   `graph_diameter()` measures the longest path length
-   Removed `node_smallworld()` and added `graph_smallworld()`, which works with both one- and two-mode networks (fixed #214)

## Motifs

-   Added some guidance to the naming convention used in `node_quad_census()`

## Models

-   Extended `network_reg()`'s formula-based system
    -   `network_reg()` can now handle binary and multiple categorical variables (factors and characters, closed #211);
    -   `network_reg()` can now manage interactions specified in the common syntax; `var1 * var2` expands to `var1 + var2 + var1:var2` (closed #163)
    -   `dist()` and `sim()` effects have been added (closed #207)
-   `network_reg()` now employs logistic regression to estimate a binary outcome and linear regression to estimate a continuous outcome (closed #184)
-   `network_reg()` now uses Dekker et al's semi-partialling procedure by default for multivariate specifications (closed #206), defaulting to *y*-permutations in the case of a single predictor (closed #208)
-   Added parallelisation to Monte Carlo based tests
    -   Refactored `network_reg()`, relying on `{furrr}` for potential parallelisation and `{progressr}` for progress reports (closed #185, #186)
    -   Refactored `test_random()` and `test_permutation()`, relying on `{furrr}` for potential parallelisation and `{progressr}` for progress reports; note that `nSim` argument now `times` (closed #199)
-   Added `{broom}` S3 methods for `netlm` and `netlogit` class objects (closed #183)
    -   `tidy()` extracts coefficients and related values
    -   `glance()`extracts model-level values such as `R^2`
-   Added plot method for `netlm` and `netlogit` class objects (closed #216), which plots the empirical distribution for each test statistic, indicates percentiles relating to common critical values, and superimposes the observed coefficients
-   Added plot method for `cug_test` and `qap_test` class objects, which plots the empirical distribution, highlighting tails beyond some critical value (closed #213), and superimposing the observed coefficient and, possibly, 0
-   Relabelled some classes to avoid loading conflicts with `{sna}`
    -   `print.block_model()` replaces `print.blockmodel()`
    -   `plot.block_model()` replaces `plot.blockmodel()`
-   Reduced the number of simulations used in tests, examples, and vignettes to avoid CRAN warnings

## Data

-   Updated several names of datasets for consistency and conciseness
    -   `ison_southern_women` instead of `southern_women`
    -   `ison_brandes` instead of `brandes`
    -   `ison_networkers` instead of `ison_eies`
    -   `ison_algebra` instead of `ison_m182`
    -   `ison_adolescents` instead of `ison_coleman`
-   Extended several datasets
    -   `mpn_elite_mex` is extended with data from Pajek and with help from Frank Heber
    -   `ison_networkers` becomes named with information from `{tnet}`
-   Elaborated documentation of most `mpn_*` and `ison_*` datasets, including references/sources

# migraph 0.8.13

## Modelling

-   Closed #149 by adding extra column to node_tie_census in `cluster_structural_equivalence()` for isolates
    -   Note that this renders all isolates structurally equivalent

# migraph 0.8.12

## Package

-   Closed #168 by adding `{patchwork}` to suggested packages in DESCRIPTION
-   Updated function reference page on website

## Manipulation

-   Updated `add_` functions
    -   Closed #178 by adding name to existing edges when further edges added in `mutate_edges()`
    -   Closed #179 by inferring an attribute vector is for one of the two modes where possible in `add_node_attributes()`
-   Added `is_` methods: `is_multiplex()`, `is_uniplex()`, `is_acyclic()`
-   Added `edge_` functions to identify edges by properties: `edge_mutual()`, `edge_multiple()`, `edge_loop()`

# migraph 0.8.11

## Import and export

-   Fixed #172 by removing redundant header argument in `read_nodelist()` and `read_edgelist()`

## Package

-   Fixed #173 by extending `as_network()` method to convert correctly form an `{igraph}` to a `{network}` object.
-   Removed `ggraphgrid()` documentation

# migraph 0.8.10

## Import and export

-   Replaced xlsx dependency in `read_edgelist()` and `read_nodelist()` to readxl to avoid Java dependency
-   Replaced xlsx dependency in `write_edgelist()` and `write_nodelist()` to avoid Java dependency
    -   Note that these functions will now export to .csv rather than .xlsx

## Manipulation

-   Fixed direction recognition bug in `as_network()`, `as_igraph()`, and `is_directed()`

# migraph 0.8.9

## Package

-   Closed #139 by adding vignette on importing and connecting data

## Import and export

-   Added `read_` and `write_` functions and updated documentation
    -   Closed #137 by adding `read_edgelist()` for importing edgelists from Excel and csv files
    -   Closed #170 by adding `read_pajek()` for importing .net and .paj files
    -   Added `write_edgelist()`, `write_nodelist`, `write_pajek()`, and `write_ucinet()` for exporting into various file formats (Excel, csv, Pajek, and UCINET)
    -   Closed #140 by adding links to further data resources

## Manipulation

-   Added `is_graph()` to check if an object is a graph or not
-   Extended `as_network()` to retain attributes
-   Fixed bugs in `as_` and `to_` functions
    -   Fixed bug in `as_` functions to convert from dataframes instead of tibbles
    -   Fixed bug in conversion from network to igraph object in `as_igraph()` function
    -   Fixed bug in `to_undirected()` function to work with network objects
    -   Fixed bug in `to_main_component()` function so that it retains vertex attributes in network objects
-   Added `edge_attribute()` to grab a named edge attribute from a graph/network
-   Updated `to_unweighted()` to prevent conversion of network object into igraph object when deleting weights

## Measures

-   Closed #143 by adding nodal summary by cluster function `summarise_statistics()`

## Modelling

-   Fixed `network_reg()` example

## Visualisation

-   Closed #117 by updating the node/edge/arrow size limits in `autographr()`

# migraph 0.8.8

## Package

-   Added start to network linear model part of practical 7 vignette
-   Thanks to @BBieri for adding many tests and working on igraph\<-\>network interchange

## Data

-   Added `ison_eies` dataset for use in practical 7 vignette

## Manipulation

-   The `as_matrix()` method for networks now works with two-mode and weighted networks
-   The `as_igraph()` method for matrices now checks for weights independently of coercion
-   The `as_igraph()` method for networks now works with two-mode and weighted networks
-   The `as_network()` method for matrices now works with two-mode and weighted networks
-   The `as_network()` method for edgelists, igraph, and tidygraphs now works with weighted networks
-   Added `to_unnamed()` method for edge lists
-   Added `to_simplex()` method for matrices
-   Added `to_main_component()` method for networks
-   Added `to_multilevel()` method for matrices
-   `mutate_edges()` now coalesces rows of edges

## Measures

-   Fixed bug where clusters were not being reported in the correct order in `graph_blau_index()`

## Modelling

-   Fixed one-mode bug with `generate_permutation()` and thus `test_permutation()`
-   Renamed `netlm()` to `network_reg()` to avoid frustrating conflicts
    -   `network_reg()` now accepts migraph-consistent objects
    -   `network_reg()` now accepts formula terms such as `ego()`, `alter()`, and `same()`

# migraph 0.8.7

## Package

-   Added new issue templates and refined the wording in existing templates
-   Improved documentation across many help pages
-   Closed #146 by adding vignette on homophily

## Data

-   Added `generate_permutation()` which takes an object and returns an object with the edges permuted, but retaining all nodal attributes
-   Made `generate_random()` also work with an existing object as input, in which it will return a random graph with the same dimensions and density
-   Consolidated data scripts

## Manipulation

-   Added `mutate_edges()` for adding new edges as attributes to existing edges in an object

## Measures

-   Closed #159 by fixing bug in `graph_blau_index()`
-   Closed #157 by fixing bug in `graph_ei_index()`
-   Closed #156 and #158 by fixing bugs with `test_random()` (defunct `test_cug()`)

## Visualisation

-   Closed #148 and #153 by making all `autographr()` arguments take variable names in quotation marks

# migraph 0.8.6

## Package

-   Closed #75 by updating the README

## Manipulation

-   Added some functions for grabbing key information from objects
    -   `node_names()` for quickly accessing node labels
    -   `node_attribute()` for quickly accessing a certain nodal attribute
    -   `edge_weights()` for quickly accessing edge weights
    -   `graph_nodes()` for quickly accessing a count of nodes in the graph, note that for two-mode networks this will be a vector of length 2
    -   `graph_edges()` for quickly accessing a count of edges in the graph
    -   `graph_dimensions()` is currently a copy of `graph_nodes()`
-   Added some functions for adding key information to objects
    -   `add_node_attributes()` for adding particular nodal attributes
    -   `add_edge_attributes()` for adding edges from another graph
    -   `copy_edge_attributes()` for copying all nodal attributes from another graph
-   Improved twomode and weighted handling of several functions

## Measures

-   Added diversity functions
    -   `graph_blau_index()` for summarising diversity of an attribute in a network or group
    -   `graph_ei_index()` for summarising diversity of an attribute over a network's ties

## Modelling

-   Closed #119 by adding `node_quad_census()`, especially useful for two-mode blockmodelling
-   Closed #95 and #120 by adding `graph_mixed_census()`
-   Closed #97 by adding test functions
    -   `test_random()` carries out a conditional uniform graph (CUG) test
    -   `test_permutation()` carries out a quadratic assignment procedure (QAP) test

## Visualization

-   Closed #135 by reexporting `aes()` from `{ggplot2}`
-   Added `node_shape` option to `autographr()`

# migraph 0.8.5

## Package

-   Updated various URLs in the vignettes to pass CRAN tests
-   Reduce number of layout examples to avoid examples taking too long to run

# migraph 0.8.4

## Classes

-   Closed #128 by adding `as_edgelist()` methods for converting other objects into edgelists
    -   Note that this currently returns a tibble
-   Using `to_unnamed()` on 'network' objects now operates on them directly
-   Elaborated `to_` documentation significantly
-   Fixed bug in `to_onemode()` that was tripping `blockmodel()` on networks that are already one-mode
-   Added `is_connected()` to test whether network is connected, `method =` argument can be specified as `weak` or `strong`

## Data

-   Added `create_tree()` and `create_lattice()`, and made `create_star()` a bit faster for one-mode networks
-   Added `generate_smallworld()` and `generate_scalefree()`, though only for one-mode networks currently

## Measures

-   Added rounding to centralization measures, by default `=2`
-   Closed #109 by adding centrality vignette

## Modelling

-   Added `graph_dyad_census()` for more graph profile options
-   Fixed bug with `blockmodel_concor()` when an object was of class 'igraph' but not 'tbl_graph'
-   Fixed bug in how `blockmodel()` was treating two-mode networks
-   Closed #116 by offering both `"elbow"` and `"strict"` methods for *k*-identification
    -   Fixed bug in elbow method that biased heavily bipartitioned data
-   Closed #131 by refactoring `ggidentify_clusters()` for speed
    -   Takes now roughly half the time (see issue for details)

## Visualization

-   Added `ggdistrib()` for easy plotting of degree and other node score distributions
-   Reexported `ggsave()`, `xlab()` and `ylab()` from `{ggplot2}` for easier plot annotation

# migraph 0.8.3

## Package

-   Closed #108 by adding cohesion and community vignette

## Classes

-   Fixed #122 by retaining edge weights from igraph in `as_matrix()` where available

## Measures

-   Split `graph_equivalency()` into the same for two-mode networks and `graph_congruency()`for three-mode (two two-mode) networks
-   Added option for `graph_reciprocity()` method
-   Added `graph_components()` and `node_components()`

## Modelling

-   Fixed #113 by retaining node labels through census functions
-   Closed #114 by transposing `node_tie_census()` output so that it's consistent with `node_triad_census()` and future node_census functions
-   Closed #121 by renaming `cluster_triad_census()` to `group_triad_census()`
-   Added `group_tie_census()`

## Visualization

-   Added option to `autographr()` for plotting convex/concave hulls
-   Closed #124 by making `ggraphgrid()` a set of layout functions:
    -   `layout_tbl_graph_frgrid()` or `autographr(object, "frgrid")` for snapping Fruchterman-Reingold to a grid
    -   `layout_tbl_graph_kkgrid()` or `autographr(object, "kkgrid")` for snapping Kamada-Kawai to a grid
    -   `layout_tbl_graph_gogrid()` or `autographr(object, "gogrid")` for snapping graph optimisation to a grid
    -   `ggraphgrid()` has been deprecated

## Data

-   Fixed some `ison_m182` documentation

# migraph 0.8.2

## Package

-   Fixed CRAN package check dependencies bug where 'knitr' and 'rmarkdown' were listed as Imports without being used in the package

## Classes

-   Fixed bug where bipartite edge lists were not being recognised as a twomode network by `as_igraph()`
-   Fixed bug where `to_uniplex()` was not returning a weighted graph

## Models

-   Fixed bug where `blockmodel()` was not retaining node names in all parts of the object structure

## Visualization

-   Closed #107 by choosing better brewer pallette (though note this is not a very deep pallette with only 9 colors)

## Vignettes

-   Expanded on the blockmodelling vignette with more intro, discussion, interpretation clues

# migraph 0.8.1

## Package

-   Fixed codecov url bug
-   Removed several package dependencies by moving `plot_releases()` to another package
-   Made many dependencies more explicit
-   Entire package 'linted'

## Classes

-   Added `is_signed()` to logically test whether the network is a signed network
-   Added `to_unsigned()` for extracting networks of either "positive" or "negative" ties from a signed network
-   Added `tbl_graph` methods for all other `to_` functions
-   Reexported `activate()` from `{tidygraph}`

## Visualisation

-   Added sensible plotting defaults for signed networks in `autographr()`
-   Removed `plot_releases()` from this package

## Measures

-   Refactored `graph_balance()` to be much faster, following David Schoch's `{signnet}` package (see that package for further extensions)

## Data

-   Updated the edge 'sign' attribute of `ison_marvel_relationships` to be a double (`-1`/`1`) to be compatible with the new `graph_balance()` and `{signnet}`

# migraph 0.8.0

## Classes

-   Fixed coercion to `{igraph}` from data frames and updated read script
-   Added `to_main_component()` to extract the main component of a network
-   Added `to_onemode()` for moving to multimodal igraph objects
-   Added `to_uniplex()` method to delete edge types and their edges from multiplex networks
-   Added `to_simplex()` method to delete loops from a network
-   Added `to_named()` method for randomly naming unlabeled networks

## Data

-   Added `ison_mm`, `ison_mb`, `ison_bm`, and `ison_bb` projection illustration data
-   Added `ison_karateka` community detection illustration data
-   Added `ison_marvel_teams` and `ison_marvel_relationships` datasets
-   Added `ison_m182` dataset of friends, social and task ties between 16 anonymous students
-   Renamed `adolescent_society` dataset to `ison_coleman`for consistency
-   Data now listed at the bottom of the website References page

## Measures

-   Added `graph_eigenvector()` for one mode networks
-   Added `graph_balance()` for measuring structural balance
-   Added `node_tie_census()`, `node_triad_census()`, `cluster_triad_census()`, and `graph_triad_census()`
-   Separated out `graph_clustering()` into the cohesion measures `graph_density()`, `graph_reciprocity()`, `graph_transitivity()`, and `graph_equivalence()`
-   Fixed `node_smallworld()` to use separated cohesion measures

## Models

-   Added `blockmodel()` which masks its `{sna}` namesake but has the advantages of working with two-mode networks and retaining node names where available
    -   Added `cluster_structural_equivalence()` and `cluster_regular_equivalence()` as bases for blockmodelling
    -   Added `reduce_graph()` for creating a network from a blockmodel
-   Added first vignette on structural holes, structural equivalence and regular equivalence blockmodelling

## Visualization

-   Added `autographr()` for plotting graphs with sensible defaults
    -   Uses a more contrastive discrete palette when some nodal attribute is given
    -   Uses an alpha for edges, and edges will now be sized by edge weight, where available
    -   Uses node labels, sans borders, where available
    -   Uses different shaped nodes, and different fonts, for different node sets
    -   Removed `ggraphlabel()` since core functionality now provided by autographr
-   Added ability for `ggidentify()` to identify the node with the highest value of a specified node-level measure
-   Added a couple of more specific visualization functions
    -   Added `ggatyear()` for subsetting and plotting edgelists at year
    -   Updated `gglineage()` to return a graph colored according to lineage
        -   Added tick marks
-   Added several more specific functions for diagnosing and visualising blockmodels
    -   Added `ggtree()` for neatly visualising hierarchical clusters
    -   Added `ggidentify_clusters()` for identifying which number of clusters is most appropriate following the elbow method
-   Fixed bug related to `ggraph::theme_graph()` present in a few different visualisation functions

# migraph 0.7.2

## Data

-   Added `brandes` dataset for teaching centrality measures
-   Added `adolescent_society` dataset for teaching friendship paradox
-   Added `read_edgelist()` for importing Excel-created edgelists directly

## Visualization

-   Added `ggraphlabel()` for one-function (1F) plotting label-based network graphs
-   Added `ggevolution()` for 1F-plotting begin/end graph comparisons
-   Added `ggraphgrid()` for 1F snap-to-grid graph layouts based on Fruchterman-Reingold or Kamada-Kawai
-   Added `ggidentify()` for 1F identifying nodes with maximum scores based on some arbitrary function

## Manipulation

-   Added `to_undirected()` for symmetrising networks of all types
-   Made existing `to_` functions S3 methods

# migraph 0.7.1

## Classes

-   Fixed Unicode char bug in coercion documentation

# migraph 0.7.0

## Classes

-   Closed #100 by converting `as_` coercion functions to S3 methods

    -   Added a little more readable documentation
    -   Fixed bug with `as_matrix()` weighting
    -   Fixed bug with `as_tidygraph()`

## Visualisation

-   Closed #92 by adding `gglineage()` for graphing a citation network through time
-   Closed #99 by adding `ggevolution()` for graphing two timepoints of the same network side by side
-   Closed #102 by adding `ggraphgrid()` for locking a graph to a grid
-   Slight improvements to `plot.igraph()` defaults

## Analysis

-   Added tidygraph lookups to `node_` functions

# migraph 0.6.6

## Classes

-   Fixed bug in `as_matrix()` with frame matrix by dropping (rarely necessary) functionality

    -   Improved handling of weights column in three-column edgelists
    -   Improved documentation of `as_` functions

## Visualisation

-   Fixed bugs in `plot_releases()` with more graceful handling of http errors

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

-   Fixed bug in how `as_network()` sometimes coerced two-mode networks into much larger dimension matrices

-   Added more `is_` tests for class-independent property tests

    -   Added `is_weighted()`
    -   Added `is_directed()`
    -   Added `is_labelled()`

## Data

-   Added @csteglich 's `read_ucinet()` and `write_ucinet()` functions

    -   `read_ucinet()` offers a file-picker when file path unknown
    -   `read_ucinet()` now imports to an igraph-class object by default, with an argument to allow other alternatives
    -   `write_ucinet()` works with all migraph-compatible objects

-   Updated `mpn_bristol` documentation

-   Added `create_star()` function

    -   Added in-star/out-star option via `directed =` argument
    -   Updated `create_` documentation

-   Renamed `sample_affiliation()` to `generate_random()`

    -   Rewrote `generate_random()` to be able to generate random one- or two-mode networks
    -   Updated documentation

## Models

-   Added test for `print.blockmodel()`

# migraph 0.6.2

2021-04-13

## Package

-   Reran `usethis::use_mit_license("James Hollway")`. MIT License file now contains only the standard two lines.
-   Removed `\dontrun` from examples. `netlm()` now runs in \<5 seconds.
-   Fixed missing website item

# migraph 0.6.1

2021-04-11

## Package

-   Closed #21 by elaborating DESCRIPTION file in preparation for CRAN submission
-   Updated several old URLs in documentation

## Classes

-   Closed #85 by adding `as_network()` to coerce objects into network class
-   Modified other coercion functions to also work with network class objects

# migraph 0.6.0

2021-03-03

## Package

-   Moved package's Github repository from `jhollway/` to `snlab-ch/` organisation
-   Trimmed some package dependencies and added others

## Data

-   Elaborated documentation for the remainder of the datasets

    -   Now all datasets in this package are titled with whether they are one-mode, two-mode, or three-mode

## Measures

-   Fixed bug in `graph_degree()` where data was hard-coded in

## Models

-   Closed #18 by adding `blockmodel_concor()` for employing the CONCOR algorithm to blockmodel both one-mode and two-mode networks

    -   Added a new print method for "blockmodel"-class objects based on the `print.blockmodel()` method in the `{sna}` package that also prints blockmodel results for two-mode networks consistently
    -   Added a new plot method for "blockmodel"-class objects that leverages `{ggplot2}` for pretty plotting and that better inherits names from the underlying object

# migraph 0.5.0

2021-02-06

## Package

-   Closed #81 by making `{migraph}` depend on R versions 4.0 or above
-   Updated PR template

## Classes

-   Added functions for class conversion between migraph-consistent graph formats
-   `as_matrix()` function to coerce objects into an adjacency or incidence matrix class
-   `as_igraph()` function to coerce objects into an `{igraph}` graph class
-   `as_tidygraph()` function to coerce objects into an `{tidygraph}` tbl_graph class
-   Closed #79 by adding `is_twomode()` function to check whether network is two-mode on all object types

## Data

-   Renamed several datasets and elaborated their documentation

    -   `mpn_mexicanpower` was renamed to `mpn_elite_mex`
    -   `mpn_powerelite` was renamed to `mpn_elite_usa_advice`
    -   `mpn_opensecrets` was renamed to `mpn_elite_usa_money`

-   Reconstructed several creation functions to take universal (one-mode/two-mode) input: specifying `n = 5` creates a one-mode network, while specifying `n = c(5, 5)` creates a two-mode network

    -   Added `create_empty()`
    -   Added `create_complete()`
    -   Closed #65 by extending `create_ring()` to create rings of varying breadth
    -   Closed #66 by extending `create_components()` (renamed from `create_silos()`) to create networks with varying numbers of components
    -   Added `sample_affiliation()` for random two-mode networks
    -   Removed `create_match()` and `create_nest()`

## Measures

-   Renamed `centrality_` functions with `node_` prefix and ensured they all also wrapped one-mode measures

    -   `centrality_degree()` renamed to `node_degree()`
    -   `centrality_closeness()` renamed to `node_closeness()`
    -   `centrality_betweenness()` renamed to `node_betweenness()`
    -   Closed #31 by adding `node_eigenvector()`

-   Re-added `node_constraint()` for calculating Burt's constraint measure for one- and two-mode networks

-   Re-added `node_smallworld()` for calculating Watts-Strogatz measure of small-worldness for two-mode networks

-   Closed #32 by re-adding centralization functions for one- and two-mode networks

    -   `graph_degree()` for degree centralization
    -   `graph_closeness()` for closeness centralization
    -   `graph_betweenness()` for betweenness centralization

-   Re-added `graph_clustering()` for calculating (see Knoke et al 2021):

    -   transitivity on one-mode networks
    -   shared four-cycles on two-mode networks
    -   congruent four-cycles on three-mode networks

## Models

-   Re-added `netlm()` for performing linear regression for multimodal network data

    -   Closed #76 by changing `netlm()` to accept a formula-based input
    -   Closed #77 by adding `print.summary.netlm()` for `netlm()` regressions

## Visualization

-   Closed #82 by re-adding a version `plot.igraph()` with sensible defaults for two-mode networks

# migraph 0.4.1

2021-01-11

## Package

-   pkgdown now deploys after release
-   Reexported a number of `{igraph}` and `{tidygraph}` functions for internal use
-   Completed some `convert_` and `project_` documentation

## Data

-   Updated mpn\_ data source references

## Analysis

-   Added centrality measures that take (and if necessary return) matrix, igraph, or tidygraph objects, and offer a correct normalization for two-mode networks

    -   Added `centrality_degree()`
    -   Added `centrality_closeness()`
    -   Added `centrality_betweenness()`

# migraph 0.4.0

2021-01-08

## Package

-   Package name change from roctopus to `{migraph}`

    -   Closed #50 with new logo

-   Now builds Linux binary too

## Manipulation

-   Added `project_rows()` and `project_cols()` to make it easier to project two-mode networks in different formats (matrix, igraph, tidygraph) into projected versions in the same format
-   Closed #30 with conversion from different data frame formats, e.g. weighted and unweighted edgelists, into an incidence matrix with `as_incidence_matrix()`

## Data

-   Renamed data related to the book "Multimodal Political Networks" with "mpn\_" prefix

# roctopus 0.3.0

2020-11-06

## Data

-   Data creation updated for `{tidygraph}` defaults

    -   Renamed `create_lattice()` to `create_chain()` to avoid conflicts with `tidygraph::create_lattice()`
    -   Renamed `create_poles()` to `create_silos()`
    -   Renamed `create_random()` to `play_twomode()` to avoid conflicts with `tidygraph::play_bipartite()`
    -   Added export options for all `create_` and `play_` functions as `tbl_graph` (default), `igraph`, and base matrix
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

-   Added `row_project()` and `col_project()` functions to make it easier to remember project directions

# roctopus 0.2.3

2020-07-19

## Data

-   Added `mexicanpower` dataset
-   Added `senate112` datasets
-   Updated `evs` datasets to matrices

## Package

-   Added `df_to_mat()` function for converting regular adjacency and incidence data frames into matrices

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

-   Added `twomode_modularity()` to calculate modularity in two-mode networks

## Visualization

-   Added `plot_multilevel()` that rotates a force-directed `igraph` plot in three dimensions to reveal multilevel structure
-   Added `plot_globalnet()` to map a multilevel network on to a javascript, rotatable global

# roctopus 0.0.3

2018-08-25

## Analysis

-   Added `twomode_smallworld()` to calculate observed/expected clustering, observed/expected path-length, and the observed/expected clustering ratio
-   Added `twomode_2x2()` to identify dominance and coherence values for networks over time
-   Updated `twomode_coherence()` to allow for introduction of second-mode attributes
-   Renamed `twomode_fragmentation()` to `twomode_components()`

## Visualisation

-   Added `plot_2x2()` to plot values through a two-by-two matrix

# roctopus 0.0.2

2018-08-14

## Package

-   Renamed package to `roctopus`
-   Added two-mode @family tag to documentation

## Analysis

-   Added `twomode_fragmentation()` to calculate number of components in two-mode networks and identify their membership
-   Added `twomode_dominance()` to allow an nodal attribute to be passed to the function to weight the centralization measure
-   Added `twomode_coherence()` to calculate Jaccard similarity

## Visualisation

-   Added `plot_twomode()`, which wraps `plot(igraph::graph_from_incidence_matrix())` with some useful defaults

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

-   Added `twomode_clustering()` to calculate percentage of three-paths closed by four-paths
-   Added `twomode_lattice()` to create two-mode lattices
-   Added `twomode_centralization_degree()` to calculate degree centralization in two-mode networks, for rows, columns, or both
-   Added `twomode_centralization_between()` to calculate betweenness centralization in two-mode networks
-   Added `twomode_constraint()` to calculate network constraint in two-mode networks
-   Added `arrange.vars()` to rearrange variables by position
