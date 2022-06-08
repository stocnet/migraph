#' Network/topological features
#' @inheritParams is
#' @name features
#' @family measures
NULL

#' @describeIn features Returns small-world metrics for one- and two-mode networks.
#' @param times Integer of number of simulations.
#' @examples
#' graph_smallworld(ison_southern_women)
#' graph_smallworld(ison_brandes)
#' @seealso \code{\link{graph_transitivity}} and \code{\link{graph_equivalency}}
#' for how clustering is calculated
#' @references 
#' Watts, Duncan J., and Steven H. Strogatz. 1998. 
#' “Collective Dynamics of ‘Small-World’ Networks.” 
#' _Nature_ 393(6684):440–42.
#' @export
graph_smallworld <- function(object, times = 100) {
  
  if(is_twomode(object)){
    obsclust <- graph_equivalency(object)
    expclust <- mean(vapply(1:times, 
                            function(x) graph_equivalency(generate_random(object)),
                            FUN.VALUE = numeric(1)))
  } else {
    obsclust <- graph_transitivity(object)
    expclust <- mean(vapply(1:times, 
                            function(x) graph_transitivity(generate_random(object)),
                            FUN.VALUE = numeric(1)))
  }
  
  obspath <- graph_length(object)
  exppath <- mean(vapply(1:times, 
                         function(x) graph_length(generate_random(object)),
                         FUN.VALUE = numeric(1)))
  
  make_graph_measure((obsclust/expclust)/(obspath/exppath),
                     object)
}

#' @describeIn features Returns the structural balance index on 
#'   the proportion of balanced triangles,
#'   ranging between `0` if all triangles are imbalanced and 
#'   `1` if all triangles are balanced.
#' @param method Default is "triangles".
#' @source `{signnet}` by David Schoch
#' @examples
#' graph_balance(ison_marvel_relationships)
#' @export
graph_balance <- function(object, method = "triangles") {
  
  method <- match.arg(method)
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
  if (method == "triangles") {
    tria_count <- count_signed_triangles(g)
    make_graph_measure(unname((tria_count["+++"] + tria_count["+--"])/sum(tria_count)),
                       object)
  }
}

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

#' @describeIn features Returns modularity of one- or two-mode networks
#' @param membership A vector of partition membership
#' @param resolution A proportion indicating the resolution scale.
#'   By default 1.
#' @examples 
#' graph_modularity(ison_adolescents, 
#'   node_kernighanlin(ison_adolescents))
#' graph_modularity(ison_southern_women, 
#'   node_kernighanlin(ison_southern_women))
#' @references 
#' Murata, Tsuyoshi. 2010. Modularity for Bipartite Networks. 
#' In: Memon, N., Xu, J., Hicks, D., Chen, H. (eds) 
#' _Data Mining for Social Network Data. Annals of Information Systems_, vol 12. 
#' Springer, Boston, MA. 
#' \doi{10.1007/978-1-4419-6287-4_7}
#' @export
graph_modularity <- function(object, 
                             membership = NULL, 
                             resolution = 1){
  if(!is_graph(object)) object <- as_igraph(object)
  if(is_twomode(object)){
    make_graph_measure(igraph::modularity(to_multilevel(object), 
                       membership = membership,
                       resolution = resolution), object)
  } else make_graph_measure(igraph::modularity(object, 
                            membership = membership,
                            resolution = resolution),
                            object)
}
