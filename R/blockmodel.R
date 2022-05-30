#' Blockmodelling
#' @inheritParams is
# #' Passing the function a list apply the algorithm to the joined matrices.
#' @param p An integer representing the desired number of partitions.
#' @param cutoff A value between 0 and 1 used to determine convergence.
#' @param max.iter An integer representing the maximum number of iterations.
#' @param block.content A string indicating which method to use for
#' calculating block content.
#' Options are: "density", "sum", "meanrowsum", "meancolsum",
#' "median", "min", "max".
#' @param block_labels A character vector manually providing labels
#' for the blocks in the blockmodel
#' @param clusters the vector of cluster membership for the blockmodel
#' @param blockmodel a blockmodel object
#' @name blockmodel
#' @source \url{https://github.com/aslez/concoR}
#' @references Breiger, R.L., Boorman, S.A., and Arabie, P.  1975.  
#' An Algorithm for Clustering Relational Data with Applications to 
#' Social Network Analysis and Comparison with Multidimensional Scaling. 
#' \emph{Journal of Mathematical Psychology}, 12: 328--383.
#' @importFrom stats cor median
#' @examples 
#' mex_concor <- blockmodel_concor(mpn_elite_mex)
#' mex_concor
#' plot(mex_concor)
#' usa_concor <- blockmodel_concor(mpn_elite_usa_advice)
#' usa_concor
#' plot(usa_concor)
NULL

#' @rdname blockmodel
#' @importFrom sna blockmodel
#' @export
blockmodel <- function(object, clusters){
  # if(is_twomode(object)) object <- to_onemode(object)
  mat <- as_matrix(to_onemode(object))
  out <- sna::blockmodel(mat, clusters)
  if (is_twomode(object)) {
    out[["modes"]] <- 2
    dims <- dim(as_matrix(object))
    nodes1 <- 1:dims[1]
    nodes2 <- (dims[1] + 1):ncol(out$blocked.data)
    out[["blocked.data"]] <- out$blocked.data[nodes1,nodes2]
    memb <- out$block.membership
    out[["block.membership"]] <- NULL
    out[["block.membership"]][["nodes1"]] <- memb[nodes1]
    out[["block.membership"]][["nodes2"]] <- memb[nodes2] - max(memb[nodes1])
    out[["plabels"]] <- NULL
    rownames(out[["blocked.data"]]) <- out[["plabels"]][["nodes1"]] <- rownames(as_matrix(object))
    colnames(out[["blocked.data"]]) <- out[["plabels"]][["nodes2"]] <- colnames(as_matrix(object))
    orders <- out[["order.vector"]]
    out[["order.vector"]] <- NULL
    out[["order.vector"]][["nodes1"]] <- orders[nodes1]
    out[["order.vector"]][["nodes2"]] <- orders[nodes2] - max(orders[nodes1])
  } else {
    out[["modes"]] <- 1
    rownames(out[["blocked.data"]]) <- out[["plabels"]] <- rownames(mat)
    colnames(out[["blocked.data"]]) <- out[["glabels"]] <- colnames(mat)
  }
  out
}

#' @rdname blockmodel
#' @param x An object of class "block_model"
#' @param ... Additional arguments passed to generic print method
#' @export
print.block_model <- function(x, ...) {
  if (is.null(x$modes)) {
    sna::print.blockmodel(x)
  } else if (x$modes == 1) {
    cat("\nNetwork Blockmodel:\n\n")
    cat("Block membership:\n\n")
    if (is.null(x$plabels)) {
      plab <- seq_len(x$block.membership)[x$order.vector]
    } else if (is.list(x$plabels)) {
      plab <- x$plabels[[1]]
    } else {
      plab <- x$plabels
    }
    temp <- matrix(x$block.membership, nrow = 1)
    dimnames(temp) <- list("", plab)
    print(temp[1, order(x$order.vector)])
  } else {
    cat("\nNetwork Blockmodel:\n\n")
    cat("Block membership:\n\n")
    cat("  First nodeset:\n\n")
    if (is.null(x$plabels)) {
      plab <- seq_len(x$block.membership$nodes1)[x$order.vector$nodes1]
    } else {
      plab <- x$plabels[[1]]
    } 
    temp <- matrix(x$block.membership$nodes1, nrow = 1)
    dimnames(temp) <- list("", plab)
    print(temp[1, order(x$order.vector$nodes1)], ...)
    cat("\n  Second nodeset:\n\n")
    if (is.null(x$plabels)) {
      plab <- seq_len(x$block.membership$nodes2)[x$order.vector$nodes2]
    } else {
      plab <- x$plabels[[2]]
    } 
    temp <- matrix(x$block.membership$nodes2, nrow = 1)
    dimnames(temp) <- list("", plab)
    print(temp[1, order(x$order.vector$nodes2)], ...)
  }
  cat("\nReduced form blockmodel:\n\n")
  if (length(dim(x$block.model)) > 2) {
    for (i in 1:dim(x$block.model)[1]) {
      temp <- x$block.model[i, , ]
      dimnames(temp) <- list(x$rlabels, x$rlabels)
      cat("\t", x$glabels[i], "\n")
      print(temp, ...)
      cat("\n")
    }
  }
  else {
    temp <- x$block.model
    dimnames(temp) <- list(x$rlabels, x$rlabels)
    cat("\t", x$glabels, "\n")
    print(temp, ...)
  }
}

#' @rdname blockmodel
#' @export
reduce_graph <- function(blockmodel, block_labels = NULL){
  reduced <- igraph::graph_from_adjacency_matrix(blockmodel$block.model,
                                                 weighted = T)
  reduced <- igraph::delete_edges(reduced,
                                  which(is.nan(igraph::E(reduced)$weight)))
  if (!is.null(block_labels)) igraph::V(reduced)$name <- block_labels
  reduced
}

#' @rdname blockmodel
#' @param node_measure A vector or matrix of node-level statistics,
#' such as centrality measures or a census.
#' @param sumFUN A function by which the values should be aggregated
#' or summarised. By default `mean`.
#' @examples 
#' summarise_statistics(node_degree(mpn_elite_mex), 
#'           cutree(cluster_structural_equivalence(mpn_elite_mex), 3))
#' summarise_statistics(node_triad_census(mpn_elite_mex), 
#'           cutree(cluster_structural_equivalence(mpn_elite_mex), 3))
#' @export
summarise_statistics <- function(node_measure, 
                                 clusters = NULL,
                                 sumFUN = mean){
  if (is.matrix(node_measure)) {
    out <- t(sapply(unique(clusters), 
                  function(x) apply(node_measure[clusters == x, ], 2, sumFUN)))
    rownames(out) <- unique(clusters)
  } else {
    out <- vapply(unique(clusters), 
                  function(x) sumFUN(node_measure[clusters == x]), FUN.VALUE = 1)
    names(out) <- unique(clusters)
  }
  out
}
