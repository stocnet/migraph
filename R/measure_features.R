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
network_core <- function(object,
                       membership = NULL){
  if(is.null(membership)) membership <- node_core(object)
  out <- stats::cor(c(as_matrix(object)), 
                    c(as_matrix(create_core(object,
                                            membership = membership))))
  make_network_measure(out, object)
}

#' @describeIn features Returns correlation between a given network
#'   and a component model with the same dimensions.
#' @examples 
#' network_factions(ison_adolescents)
#' network_factions(ison_southern_women)
#' @export
network_factions <- function(object,
                       membership = NULL){
  out <- stats::cor(c(as_matrix(object)), 
                    c(as_matrix(create_components(object,
                                                  membership = membership))))
  make_network_measure(out, object)
}

#' @describeIn features Returns modularity of one- or two-mode networks
#'    based on nodes' membership in pre-defined clusters. 
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
#' _Data Mining for Social Network Data. Annals of Information Systems_, V1ol 12. 
#' Springer, Boston, MA. 
#' \doi{10.1007/978-1-4419-6287-4_7}
#' @export
network_modularity <- function(object, 
                             membership = NULL, 
                             resolution = 1){
  if(!is_graph(object)) object <- as_igraph(object)
  if(is_twomode(object)){
    make_network_measure(igraph::modularity(to_multilevel(object), 
                                          membership = membership,
                                          resolution = resolution), object)
  } else make_network_measure(igraph::modularity(object, 
                                               membership = membership,
                                               resolution = resolution),
                            object)
}

#' @describeIn features Returns small-world metrics for one- and 
#'    two-mode networks. 
#'    Small-world networks can be highly clustered and yet
#'    have short path lengths.
#' @param times Integer of number of simulations.
#' @examples
#' network_smallworld(ison_brandes)
#' network_smallworld(ison_southern_women)
#' @seealso [network_transitivity()] and [network_equivalency()]
#'   for how clustering is calculated
#' @references 
#' Watts, Duncan J., and Steven H. Strogatz. 1998. 
#' “Collective Dynamics of ‘Small-World’ Networks.” 
#' _Nature_ 393(6684):440–42.
#' \doi{10.1038/30918}.
#' @export
network_smallworld <- function(object, times = 100) {
  
  if(is_twomode(object)){
    obsclust <- network_equivalency(object)
    expclust <- mean(vapply(1:times, 
                            function(x) network_equivalency(generate_random(object)),
                            FUN.VALUE = numeric(1)))
  } else {
    obsclust <- network_transitivity(object)
    expclust <- mean(vapply(1:times, 
                            function(x) network_transitivity(generate_random(object)),
                            FUN.VALUE = numeric(1)))
  }
  
  obspath <- network_length(object)
  exppath <- mean(vapply(1:times, 
                         function(x) network_length(generate_random(object)),
                         FUN.VALUE = numeric(1)))
  
  make_network_measure((obsclust/expclust)/(obspath/exppath),
                     object)
}

#' @describeIn features Returns the structural balance index on 
#'   the proportion of balanced triangles,
#'   ranging between `0` if all triangles are imbalanced and 
#'   `1` if all triangles are balanced.
#' @source `{signnet}` by David Schoch
#' @examples
#' network_balance(ison_marvel_relationships)
#' @export
network_balance <- function(object) {
  
  count_signed_triangles <- function(object){
    g <- as_igraph(object)
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
  
  if (!is_signed(object)) {
    stop("network does not have a sign edge attribute")
  }
  if (is_directed(object)) {
    stop("object must be undirected")
  }
  g <- as_igraph(object)
  eattrV <- igraph::get.edge.attribute(g, "sign")
  if (!all(eattrV %in% c(-1, 1))) {
    stop("sign may only contain -1 and 1")
  }
  tria_count <- count_signed_triangles(g)
  make_network_measure(unname((tria_count["+++"] + tria_count["+--"])/sum(tria_count)),
                     object)
}
