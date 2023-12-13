#' Community partitioning algorithms
#' 
#' @description
#' These functions offer different algorithms useful for partitioning
#' networks into sets of communities.
#' The different algorithms offer various advantages in terms of computation time,
#' availability on different types of networks, ability to maximise modularity,
#' and their logic or domain of inspiration.
#' @inheritParams cohesion
#' @name community
#' @family memberships
NULL

#' @describeIn community A problem-solving algorithm that seeks to maximise modularity over all possible partitions.
#' @section Optimal:
#'   The general idea is to calculate the modularity of all possible partitions,
#'   and choose the community structure that maximises this modularity measure.
#'   Note that this is an NP-complete problem with exponential time complexity.
#'   The guidance in the igraph package is networks of <50-200 nodes is probably fine.
#' @references
#' Brandes, Ulrik, Daniel Delling, Marco Gaertler, Robert Gorke, Martin Hoefer, Zoran Nikoloski, Dorothea Wagner. 2008.
#' "On Modularity Clustering", 
#' _IEEE Transactions on Knowledge and Data Engineering_ 20(2):172-188.
#' @examples
#' node_optimal(ison_adolescents)
#' @export
node_optimal <- function(.data){
  out <- igraph::cluster_optimal(manynet::as_igraph(.data)
  )$membership
  make_node_member(out, .data)
}

#' @describeIn community A greedy, iterative, deterministic
#'   partitioning algorithm that results in a graph with two 
#'   equally-sized communities
#' @references
#' Kernighan, Brian W., and Shen Lin. 1970.
#' "An efficient heuristic procedure for partitioning graphs."
#' _The Bell System Technical Journal_ 49(2): 291-307.
#' \doi{10.1002/j.1538-7305.1970.tb01770.x}
#' @examples
#' node_kernighanlin(ison_adolescents)
#' node_kernighanlin(ison_southern_women)
#' @export
node_kernighanlin <- function(.data){
  # assign groups arbitrarily
  n <- manynet::network_nodes(.data)
  group_size <- ifelse(n %% 2 == 0, n/2, (n+1)/2)
  
  # count internal and external costs of each vertex
  g <- manynet::as_matrix(manynet::to_multilevel(.data))
  g1 <- g[1:group_size, 1:group_size]
  g2 <- g[(group_size+1):n, (group_size+1):n]
  intergroup <- g[1:group_size, (group_size+1):n]
  
  g2.intcosts <- rowSums(g2)
  g2.extcosts <- colSums(intergroup)
  
  g1.intcosts <- rowSums(g1)
  g1.extcosts <- rowSums(intergroup)
  
  # count edge costs of each vertex
  g1.net <- g1.extcosts - g1.intcosts
  g2.net <- g2.extcosts - g2.intcosts
  
  g1.net <- sort(g1.net, decreasing = TRUE)
  g2.net <- sort(g2.net, decreasing = TRUE)
  
  # swap pairs of vertices (one from each group) that give a positive sum of net edge costs
  if(length(g1.net)!=length(g2.net)) {
    g2.net <- c(g2.net,0)
  } else {g2.net}
  
  sums <- as.integer(unname(g1.net + g2.net))
  # positions in sequence of names at which sum >= 0
  index <- which(sums >= 0 %in% sums)
  g1.newnames <- g1.names <- names(g1.net)
  g2.newnames <- g2.names <- names(g2.net)
  # make swaps based on positions in sequence
  for (i in index) {
    g1.newnames[i] <- g2.names[i]
    g2.newnames[i] <- g1.names[i]
  }
  
  # extract names of vertices in each group after swaps
  out <- ifelse(manynet::node_names(.data) %in% g1.newnames, 1, 2)
  make_node_member(out, .data)
}

#' @describeIn community A hierarchical, decomposition algorithm
#'   where edges are removed in decreasing order of the number of
#'   shortest paths passing through the edge,
#'   resulting in a hierarchical representation of group membership.
#' @section Edge-betweenness:
#'   This is motivated by the idea that edges connecting different groups 
#'   are more likely to lie on multiple shortest paths when they are the 
#'   only option to go from one group to another. 
#'   This method yields good results but is very slow because of 
#'   the computational complexity of edge-betweenness calculations and 
#'   the betweenness scores have to be re-calculated after every edge removal. 
#'   Networks of ~700 nodes and ~3500 ties are around the upper size limit 
#'   that are feasible with this approach. 
#' @references
#' Newman, M, and M Girvan. 2004.
#' "Finding and evaluating community structure in networks." 
#' _Physical Review E_ 69: 026113.
#' @examples
#' node_edge_betweenness(ison_adolescents)
#' @export
node_edge_betweenness <- function(.data){
  out <- suppressWarnings(igraph::cluster_edge_betweenness(
    manynet::as_igraph(.data))$membership)
  make_node_member(out, .data)
}

#' @describeIn community A hierarchical, agglomerative algorithm, 
#'   that tries to optimize modularity in a greedy manner.
#' @section Fast-greedy:
#'   Initially, each node is assigned a separate community.
#'   Communities are then merged iteratively such that each merge
#'   yields the largest increase in the current value of modularity,
#'   until no further increases to the modularity are possible.
#'   The method is fast and recommended as a first approximation 
#'   because it has no parameters to tune. 
#'   However, it is known to suffer from a resolution limit.
#' @references
#' Clauset, A, MEJ Newman, MEJ and C Moore. 
#' "Finding community structure in very large networks."
#' @examples
#' node_fast_greedy(ison_adolescents)
#' @export
node_fast_greedy <- function(.data){
  out <- igraph::cluster_fast_greedy(manynet::as_igraph(.data)
  )$membership
  make_node_member(out, .data)
}

#' @describeIn community A top-down, hierarchical algorithm.
#' @section Leading eigenvector:
#'   In each step, the network is bifurcated such that modularity increases most.
#'   The splits are determined according to the leading eigenvector of the modularity matrix.
#'   A stopping condition prevents tightly connected groups from being split further.
#'   Note that due to the eigenvector calculations involved,
#'   this algorithm will perform poorly on degenerate networks,
#'   but will likely obtain a higher modularity than fast-greedy (at some cost of speed).
#' @references
#' Newman, MEJ. 2006.
#' "Finding community structure using the eigenvectors of matrices"
#' _Physical Review E_ 74:036104.
#' @examples
#' node_leading_eigen(ison_adolescents)
#' @export
node_leading_eigen <- function(.data){
  out <- igraph::cluster_leading_eigen(manynet::as_igraph(.data)
                                       )$membership
  make_node_member(out, .data)
}

#' @describeIn community A hierarchical, agglomerative algorithm based on random walks.
#' @section Walktrap:
#'   The general idea is that random walks on a network are more likely to stay 
#'   within the same community because few edges lead outside a community.
#'   By repeating random walks of 4 steps many times,
#'   information about the hierarchical merging of communities is collected.
#' @param times Integer indicating number of simulations/walks used.
#'   By default, `times=50`.
#' @references
#' Pons, Pascal, and Matthieu Latapy
#' "Computing communities in large networks using random walks".
#' @examples
#' node_walktrap(ison_adolescents)
#' @export
node_walktrap <- function(.data, times = 50){
  out <- igraph::cluster_walktrap(manynet::as_igraph(.data), 
                                  steps=times)$membership
  make_node_member(out, .data)
  
}

#' @describeIn community A hierarchical algorithm based on the information in random walks.
#' @section Infomap:
#'   Motivated by information theoretic principles, this algorithm tries to build 
#'   a grouping that provides the shortest description length for a random walk,
#'   where the description length is measured by the expected number of bits per node required to encode the path.
#' @references
#' Rosvall, M, and C. T. Bergstrom. 2008.
#' "Maps of information flow reveal community structure in complex networks", 
#' _PNAS_ 105:1118.
#' \doi{10.1073/pnas.0706851105}
#' 
#' Rosvall, M., D. Axelsson, and C. T. Bergstrom. 2009.
#' "The map equation", 
#' _Eur. Phys. J. Special Topics_ 178: 13. 
#' \doi{10.1140/epjst/e2010-01179-1}
#' @examples
#' node_infomap(ison_adolescents)
#' @export
node_infomap <- function(.data, times = 50){
  out <- igraph::cluster_infomap(manynet::as_igraph(.data), 
                                 nb.trials = times
  )$membership
  make_node_member(out, .data)
}

#' @describeIn community A greedy, iterative, probabilistic algorithm, 
#'   based on analogy to model from statistical physics.
#' @param max_k Integer constant, the number of spins to use as an upper limit
#'   of communities to be found. Some sets can be empty at the end.
#' @param resolution The Reichardt-Bornholdt “gamma” resolution parameter for modularity.
#'   By default 1, making existing and non-existing ties equally important.
#'   Smaller values make existing ties more important,
#'   and larger values make missing ties more important.
#' @section Spin-glass:
#'   This is motivated by analogy to the Potts model in statistical physics.
#'   Each node can be in one of _k_ "spin states",
#'   and ties (particle interactions) provide information about which pairs of nodes 
#'   want similar or different spin states.
#'   The final community definitions are represented by the nodes' spin states
#'   after a number of updates.
#'   A different implementation than the default is used in the case of signed networks,
#'   such that nodes connected by negative ties will be more likely found in separate communities.
#' @references
#' Reichardt, Jorg, and Stefan Bornholdt. 2006.
#' "Statistical Mechanics of Community Detection"
#' _Physical Review E_, 74(1): 016110–14.
#' \doi{10.1073/pnas.0605965104}
#' 
#' Traag, VA, and Jeroen Bruggeman. 2008.
#' "Community detection in networks with positive and negative links".
#' @examples
#' node_spinglass(ison_adolescents)
#' @export
node_spinglass <- function(.data, max_k = 200, resolution = 1){
  out <- igraph::cluster_spinglass(manynet::as_igraph(.data), 
                                   spins = max_k, gamma = resolution,
                                   implementation = ifelse(manynet::is_signed(.data), "neg", "orig")
  )$membership
  make_node_member(out, .data)
}

#' @describeIn community An agglomerative multilevel algorithm that seeks to maximise modularity over all possible partitions.
#' @section Louvain:
#'   The general idea is to take a hierarchical approach to optimising the modularity criterion.
#'   Nodes begin in their own communities and are re-assigned in a local, greedy way:
#'   each node is moved to the community where it achieves the highest contribution to modularity.
#'   When no further modularity-increasing reassignments are possible, 
#'   the resulting communities are considered nodes (like a reduced graph),
#'   and the process continues.
#' @references
#' Blondel, Vincent, Jean-Loup Guillaume, Renaud Lambiotte, Etienne Lefebvre. 2008.
#' "Fast unfolding of communities in large networks",
#' _J. Stat. Mech._ P10008.
#' @examples
#' node_louvain(ison_adolescents)
#' @export
node_louvain <- function(.data, resolution = 1){
  out <- igraph::cluster_louvain(manynet::as_igraph(.data), 
                                resolution = resolution
  )$membership
  make_node_member(out, .data)
}

#' @describeIn community An agglomerative multilevel algorithm that seeks to maximise the Constant Potts Model over all possible partitions.
#' @section Leiden:
#'   The general idea is to optimise the Constant Potts Model, 
#'   which does not suffer from the resolution limit, instead of modularity.
#'   As outlined in the `{igraph}` package, 
#'   the Constant Potts Model object function is:
#'   
#'   \deqn{\frac{1}{2m} \sum_{ij}(A_{ij}-\gamma n_i n_j)\delta(\sigma_i, \sigma_j)}
#'   
#'   where _m_ is the total tie weight, 
#'   \eqn{A_{ij}} is the tie weight between _i_ and _j_,
#'   \eqn{\gamma} is the so-called resolution parameter,
#'   \eqn{n_i} is the node weight of node _i_,
#'   and \eqn{\delta(\sigma_i, \sigma_j) = 1} if and only if
#'   _i_ and _j_ are in the same communities and 0 otherwise.
#' @references
#' Traag, V. A., L Waltman, and NJ van Eck. 2019. 
#' "From Louvain to Leiden: guaranteeing well-connected communities", 
#' _Scientific Reports_, 9(1):5233. 
#' \doi{10.1038/s41598-019-41695-z}
#' @examples
#' node_leiden(ison_adolescents)
#' @export
node_leiden <- function(.data, resolution = 1){
  if(manynet::is_weighted(.data)){ # Traag resolution default
    n <- manynet::network_nodes(.data)
    resolution <- sum(manynet::tie_weights(.data))/(n*(n - 1)/2)
  }
  out <- igraph::cluster_leiden(manynet::as_igraph(.data), 
                                resolution_parameter = resolution
  )$membership
  make_node_member(out, .data)
}


