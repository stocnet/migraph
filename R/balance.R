#' Structural balance
#' @param object a migraph-consistent object
#' @param method one of "triangles" (the default), "walk", or "frustration".
#' @source `{signnet}` by David Schoch
#' @return "triangles" returns the proportion of balanced triangles,
#' ranging between `0` if all triangles are imbalanced and `1` if all triangles are balanced.
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
    return(unname((tria_count["+++"] + tria_count["+--"])/sum(tria_count)))
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
  tri_counts}

