# Topological features ####

#' Measures of network topological features
#' @description
#'   These functions measure certain topological features of networks:
#'   
#'   - `network_core()` measures the correlation between a network
#'   and a core-periphery model with the same dimensions.
#'   - `network_richclub()` measures the rich-club coefficient of a network.
#'   - `network_factions()` measures the correlation between a network
#'   and a component model with the same dimensions.
#'   If no 'membership' vector is given for the data, 
#'   `node_kernighanlin()` is used to partition nodes into two groups.
#'   - `network_modularity()` measures the modularity of a network 
#'   based on nodes' membership in defined clusters.
#'   - `network_smallworld()` measures the small-world coefficient for one- or 
#'   two-mode networks. Small-world networks can be highly clustered and yet
#'   have short path lengths.
#'   - `network_scalefree()` measures the exponent of a fitted
#'   power-law distribution. An exponent between 2 and 3 usually indicates 
#'   a power-law distribution.
#'   - `network_balance()` measures the structural balance index on 
#'   the proportion of balanced triangles,
#'   ranging between `0` if all triangles are imbalanced and 
#'   `1` if all triangles are balanced.
#'   - `network_change()` measures the Hamming distance between two or more networks.
#'   - `network_stability()` measures the Jaccard index of stability between two or more networks.
#' 
#'   These `network_*()` functions return a single numeric scalar or value.
#' @inheritParams cohesion
#' @param membership A vector of partition membership.
#' @name features
#' @family measures
NULL

#' @rdname features
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

#' @rdname features
#' @examples
#' network_richclub(ison_adolescents)
#' @export
network_richclub <- function(.data){
  coefs <- vector()
  temp <- .data
  for(k in seq_len(max(node_degree(temp, normalized = FALSE)))){
    richclub <- manynet::to_subgraph(temp, node_degree(temp, normalized = FALSE) >= k)
    nk <- manynet::network_nodes(richclub)
    ek <- ifelse(manynet::is_directed(temp),
                 manynet::network_ties(richclub), 
                 2*manynet::network_ties(richclub))
    coefs <- c(coefs, (ek)/(nk*(nk-1)))
  }
  
  elbow_finder <- function(x_values, y_values) {
    # Max values to create line
    # if(min(x_values)==1) x_values <- x_values[2:length(x_values)]
    # if(min(y_values)==0) y_values <- y_values[2:length(y_values)]
    max_df <- data.frame(x = c(1, min(which(y_values == 1))), 
                         y = c(min(y_values), max(y_values)))
    # Creating straight line between the max values
    fit <- stats::lm(max_df$y ~ max_df$x)
    # Distance from point to line
    distances <- vector()
    for (i in seq_len(length(x_values))) {
      distances <- c(distances,
                     abs(stats::coef(fit)[2]*x_values[i] -
                           y_values[i] +
                           coef(fit)[1]) /
                       sqrt(stats::coef(fit)[2]^2 + 1^2))
    }
    # Max distance point
    x_max_dist <- x_values[which.max(distances)]
    x_max_dist
  }
  
  coefs[is.nan(coefs)] <- 1
  out <- coefs[elbow_finder(seq_along(coefs), coefs)]
    # max(coefs, na.rm = TRUE)
  make_network_measure(out, .data)
}

#' @rdname features 
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

#' @rdname features
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
  if(is.null(membership))
    membership <- node_kernighanlin(.data)
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

#' @rdname features 
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

#' @rdname features 
#' @importFrom igraph fit_power_law
#' @examples 
#' network_scalefree(ison_adolescents)
#' network_scalefree(generate_scalefree(50, 1.5))
#' network_scalefree(create_lattice(100))
#' @export
network_scalefree <- function(.data){
  out <- igraph::fit_power_law(node_degree(.data, normalized = FALSE))
  if ("KS.p" %in% names(out)) {
    if(out$KS.p < 0.05) 
      cat(paste("Note: Kolgomorov-Smirnov test that data",
                "could have been drawn from a power-law", 
                "distribution rejected.\n"))
  }
  make_network_measure(out$alpha, .data)
}

#' @rdname features 
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
    if (igraph::is_directed(g)) {
      stop("g must be undirected")
    }
    eattrV <- igraph::edge_attr(g, "sign")
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
  eattrV <- igraph::edge_attr(g, "sign")
  if (!all(eattrV %in% c(-1, 1))) {
    stop("sign may only contain -1 and 1")
  }
  tria_count <- count_signed_triangles(g)
  make_network_measure(unname((tria_count["+++"] + tria_count["+--"])/sum(tria_count)),
                       .data)
}

# Change ####

#' Measures of network change
#' @description
#'   These functions measure certain topological features of networks:
#'   
#'   - `network_change()` measures the Hamming distance between two or more networks.
#'   - `network_stability()` measures the Jaccard index of stability between two or more networks.
#' 
#'   These `network_*()` functions return a numeric vector the length of the number
#'   of networks minus one. E.g., the periods between waves.
#' @inheritParams cohesion
#' @name periods
#' @family measures
NULL

#' @rdname periods 
#' @param object2 A network object.
#' @export
network_change <- function(.data, object2){
  if(manynet::is_list(.data)){
    
  } else if(!missing(object2)){
    .data <- list(.data, object2)
  } else stop("`.data` must be a list of networks or a second network must be provided.")
  periods <- length(.data)-1
  vapply(seq.int(periods), function(x){
    net1 <- manynet::as_matrix(.data[[x]])
    net2 <- manynet::as_matrix(.data[[x+1]])
    sum(net1 != net2)
  }, FUN.VALUE = numeric(1))
}

#' @rdname periods 
#' @export
network_stability <- function(.data, object2){
  if(manynet::is_list(.data)){
    
  } else if(!missing(object2)){
    .data <- list(.data, object2)
  } else stop("`.data` must be a list of networks or a second network must be provided.")
  periods <- length(.data)-1
  vapply(seq.int(periods), function(x){
    net1 <- manynet::as_matrix(.data[[x]])
    net2 <- manynet::as_matrix(.data[[x+1]])
    n11 <- sum(net1 * net2)
    n01 <- sum(net1==0 * net2)
    n10 <- sum(net1 * net2==0)
    n11 / (n01 + n10 + n11)
  }, FUN.VALUE = numeric(1))
}
