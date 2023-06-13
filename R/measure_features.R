#' Measures of network topological features
#' @inheritParams is
#' @param membership A vector of partition membership.
#' @name features
#' @family measures
NULL

#' @describeIn features Returns correlation between a given network
#'   and a core-periphery model with the same dimensions.
#' @examples 
#' network_core(ison_adolescents)
#' network_core(ison_southern_women)
#' @references 
#' Borgatti, Stephen P., and Martin G. Everett. 2000. 
#' “Models of Core/Periphery Structures.” 
#' _Social Networks_ 21(4):375–95.
#' \doi{10.1016/S0378-8733(99)00019-2}
#' @export
network_core <- function(.data,
                       membership = NULL){
  if(is.null(membership)) membership <- node_core(.data)
  out <- stats::cor(c(manynet::as_matrix(.data)), 
                    c(manynet::as_matrix(manynet::create_core(.data,
                                            membership = membership))))
  make_network_measure(out, .data)
}

#' @describeIn features Returns correlation between a given network
#'   and a component model with the same dimensions.
#'   If no 'membership' vector is given for the data, 
#'   `node_kernaghinlin()` is used to obtain a partition into two groups.
#' @examples 
#'   network_factions(mpn_elite_mex)
#'   network_factions(ison_southern_women)
#' @export
network_factions <- function(.data,
                       membership = NULL){
  if(is.null(membership))
    membership <- node_kernighanlin(.data)
  out <- stats::cor(c(manynet::as_matrix(.data)), 
                    c(manynet::as_matrix(manynet::create_components(.data,
                                                  membership = membership))))
  make_network_measure(out, .data)
}

#' @describeIn features Returns modularity based on nodes' membership 
#'   in pre-defined clusters.
#' @section Modularity:
#'   Modularity measures the difference between the number of ties within each community
#'   from the number of ties expected within each community in a random graph
#'   with the same degrees, and ranges between -1 and +1.
#'   Modularity scores of +1 mean that ties only appear within communities,
#'   while -1 would mean that ties only appear between communities.
#'   A score of 0 would mean that ties are half within and half between communities,
#'   as one would expect in a random graph.
#'   
#'   Modularity faces a difficult problem known as the resolution limit 
#'   (Fortunato and Barthélemy 2007).
#'   This problem appears when optimising modularity,
#'   particularly with large networks or depending on the degree of interconnectedness,
#'   can miss small clusters that 'hide' inside larger clusters.
#'   In the extreme case, this can be where they are only connected
#'   to the rest of the network through a single tie.
#' @param resolution A proportion indicating the resolution scale.
#'   By default 1.
#' @examples 
#' network_modularity(ison_adolescents, 
#'   node_kernighanlin(ison_adolescents))
#' network_modularity(ison_southern_women, 
#'   node_kernighanlin(ison_southern_women))
#' @references 
#' Murata, Tsuyoshi. 2010. Modularity for Bipartite Networks. 
#' In: Memon, N., Xu, J., Hicks, D., Chen, H. (eds) 
#' _Data Mining for Social Network Data. Annals of Information Systems_, Vol 12. 
#' Springer, Boston, MA. 
#' \doi{10.1007/978-1-4419-6287-4_7}
#' @export
network_modularity <- function(.data, 
                             membership = NULL, 
                             resolution = 1){
  if(!manynet::is_graph(.data)) .data <- manynet::as_igraph(.data)
  if(manynet::is_twomode(.data)){
    make_network_measure(igraph::modularity(manynet::to_multilevel(.data), 
                                          membership = membership,
                                          resolution = resolution), .data)
  } else make_network_measure(igraph::modularity(.data, 
                                               membership = membership,
                                               resolution = resolution),
                              .data)
}

#' @describeIn features Returns small-world metrics for one- and 
#'    two-mode networks. 
#'    Small-world networks can be highly clustered and yet
#'    have short path lengths.
#' @param times Integer of number of simulations.
#' @param method There are three small-world measures implemented:
#'   - "sigma" is the original equation from Watts and Strogatz (1998),
#'     \deqn{\frac{\frac{C}{C_r}}{\frac{L}{L_r}}}, 
#'     where \eqn{C} and \eqn{L} are the observed 
#'     clustering coefficient and path length, respectively,
#'     and \eqn{C_r} and \eqn{L_r} are the averages obtained from
#'     random networks of the same dimensions and density.
#'     A \eqn{\sigma > 1} is considered to be small-world,
#'     but this measure is highly sensitive to network size.
#'  -  "omega" (the default) is an update from Telesford et al. (2011),
#'     \deqn{\frac{L_r}{L} - \frac{C}{C_l}},
#'     where \eqn{C_l} is the clustering coefficient for a lattice graph
#'     with the same dimensions.
#'     \eqn{\omega} ranges between 0 and 1, 
#'     where 1 is as close to a small-world as possible.
#'  -  "SWI" is an alternative proposed by Neal (2017),
#'     \deqn{\frac{L - L_l}{L_r - L_l} \times \frac{C - C_r}{C_l - C_r}},
#'     where \eqn{L_l} is the average path length for a lattice graph
#'     with the same dimensions.
#'     \eqn{SWI} also ranges between 0 and 1 with the same interpretation, 
#'     but where there may not be a network for which \eqn{SWI = 1}.
#' @seealso [network_transitivity()] and [network_equivalency()]
#'   for how clustering is calculated
#' @references 
#' Watts, Duncan J., and Steven H. Strogatz. 1998. 
#'   “Collective Dynamics of ‘Small-World’ Networks.” 
#'   _Nature_ 393(6684):440–42.
#'   \doi{10.1038/30918}.
#' 
#' Telesford QK, Joyce KE, Hayasaka S, Burdette JH, Laurienti PJ. 2011. 
#'   "The ubiquity of small-world networks". 
#'   _Brain Connectivity_ 1(5): 367–75.
#'   \doi{10.1089/brain.2011.0038}.
#'   
#' Neal Zachary P. 2017. 
#'   "How small is it? Comparing indices of small worldliness". 
#'   _Network Science_. 5 (1): 30–44.
#'   \doi{10.1017/nws.2017.5}.
#' @examples
#' network_smallworld(ison_brandes)
#' network_smallworld(ison_southern_women)
#' @export
network_smallworld <- function(.data, 
                               method = c("omega", "sigma", "SWI"),
                               times = 100) {
  
  method <- match.arg(method)
  
  if(manynet::is_twomode(.data)){
    co <- network_equivalency(.data)
    cr <- mean(vapply(1:times, 
                      function(x) network_equivalency(manynet::generate_random(.data)),
                      FUN.VALUE = numeric(1)))
    if(method %in% c("omega", "SWI")){
      cl <- network_equivalency(manynet::create_ring(.data))
    }
  } else {
    co <- network_transitivity(.data)
    cr <- mean(vapply(1:times, 
                            function(x) network_transitivity(manynet::generate_random(.data)),
                            FUN.VALUE = numeric(1)))
    if(method %in% c("omega", "SWI")){
      cl <- network_transitivity(manynet::create_lattice(.data))
    }
  }
  
  lo <- network_length(.data)
  lr <- mean(vapply(1:times, 
                         function(x) network_length(manynet::generate_random(.data)),
                         FUN.VALUE = numeric(1)))
  if(method == "SWI"){
    ll <- network_length(manynet::create_ring(.data))
  }
  
  out <- switch(method,
                "omega" = (lr/lo - co/cl),
                "sigma" = (co/cr)/(lo/lr),
                "SWI" = ((lo - ll)/(lr - ll))*((co - cr)/(cl - cr)))
  make_network_measure(out,
                       .data)
}

#' @describeIn features Returns the exponent of the fitted
#'   power-law distribution.
#'   Usually an exponent between 2 and 3 indicates a power-law
#'   distribution.
#' @importFrom igraph fit_power_law
#' @examples 
#' network_scalefree(ison_adolescents)
#' network_scalefree(generate_scalefree(50, 1.5))
#' network_scalefree(create_lattice(100))
#' @export
network_scalefree <- function(.data){
  out <- igraph::fit_power_law(node_degree(.data, 
                                    normalized = FALSE))
  if(out$KS.p < 0.05) 
    cat(paste("Note: Kolgomorov-Smirnov test that data",
              "could have been drawn from a power-law", 
              "distribution rejected.\n"))
  make_network_measure(out$alpha, .data)
}

#' @describeIn features Returns the structural balance index on 
#'   the proportion of balanced triangles,
#'   ranging between `0` if all triangles are imbalanced and 
#'   `1` if all triangles are balanced.
#' @source `{signnet}` by David Schoch
#' @examples
#' network_balance(ison_marvel_relationships)
#' @export
network_balance <- function(.data) {
  
  count_signed_triangles <- function(.data){
    g <- manynet::as_igraph(.data)
    if (!"sign" %in% igraph::edge_attr_names(g)) {
      stop("network does not have a sign edge attribute")
    }
    if (igraph::is.directed(g)) {
      stop("g must be undirected")
    }
    eattrV <- igraph::get.edge.attribute(g, "sign")
    if (!all(eattrV %in% c(-1, 1))) {
      stop("sign may only contain -1 and 1")
    }
    tmat <- t(matrix(igraph::triangles(g), nrow = 3))
    if (nrow(tmat) == 0) {
      warning("g does not contain any triangles")
      return(c(`+++` = 0, `++-` = 0, `+--` = 0, `---` = 0))
    }
    emat <- t(apply(tmat, 1, function(x) c(igraph::get.edge.ids(g, 
                                                                x[1:2]), igraph::get.edge.ids(g, x[2:3]), igraph::get.edge.ids(g, 
                                                                                                                               x[c(3, 1)]))))
    emat[, 1] <- eattrV[emat[, 1]]
    emat[, 2] <- eattrV[emat[, 2]]
    emat[, 3] <- eattrV[emat[, 3]]
    emat <- t(apply(emat, 1, sort))
    emat_df <- as.data.frame(emat)
    res <- stats::aggregate(list(count = rep(1, nrow(emat_df))), 
                            emat_df, length)
    tri_counts <- c(`+++` = 0, `++-` = 0, `+--` = 0, `---` = 0)
    tmp_counts <- res[, 4]
    if (nrow(res) == 1) {
      names(tmp_counts) <- paste0(c("+", "-")[(rev(res[1:3]) == 
                                                 -1) + 1], collapse = "")
    }
    else {
      names(tmp_counts) <- apply(res[, 1:3], 1, function(x) paste0(c("+", 
                                                                     "-")[(rev(x) == -1) + 1], collapse = ""))
    }
    tri_counts[match(names(tmp_counts), names(tri_counts))] <- tmp_counts
    tri_counts
  }
  
  if (!manynet::is_signed(.data)) {
    stop("network does not have a sign edge attribute")
  }
  if (manynet::is_directed(.data)) {
    stop("object must be undirected")
  }
  g <- manynet::as_igraph(.data)
  eattrV <- igraph::get.edge.attribute(g, "sign")
  if (!all(eattrV %in% c(-1, 1))) {
    stop("sign may only contain -1 and 1")
  }
  tria_count <- count_signed_triangles(g)
  make_network_measure(unname((tria_count["+++"] + tria_count["+--"])/sum(tria_count)),
                       .data)
}
