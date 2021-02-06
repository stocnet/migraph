#' Watts-Strogatz small-world model for two-mode networks
#' 
#' Calculates small-world metrics for two-mode networks
#' @param object A matrix, igraph graph, or tidygraph object
#' @param n Number of simulations
#' @family two-mode measures
#' @family node-level measures
#' @return Returns a table of small-world related metrics for each second-mode node.
#' @details The first column of the returned table is simply the number of the second-mode column.
#' The next three columns report the observed and expected clustering, 
#' and the ratio of the former to the later.
#' The next three columns report the observed and expected path-length,
#' and the ratio of the former to the later.
#' The last column reports the ratio of the observed/expected clustering ratio
#' to the observed/expected path-length ratio, which is known as a small-world metric.
#' Expected clustering and paths is the mean of twomode_clustering and mean_distance
#' over 100 random simulations with the same row and column sums.
#' @examples
#' node_smallworld(southern_women)
#' @seealso \code{\link{graph_clustering}} for how clustering is calculated
#' @importFrom igraph graph_from_incidence_matrix mean_distance
#' @importFrom stats r2dtable
#' @export 
node_smallworld <- function(object, n=100){
  mat <- as_matrix(object)
  out <- matrix(NA, ncol(mat), 7)
  for(c in 2:ncol(mat)){
    m <- mat[, 1:c]
    g <- igraph::graph_from_incidence_matrix(m)
    out[c, 1] <- graph_clustering(m)
    out[c, 4] <- igraph::mean_distance(g)
    
    r <- stats::r2dtable(n, rowSums(m), colSums(m))
    out[c, 2] <- mean(unlist(lapply(r, graph_clustering)))
    out[c, 5] <- mean(unlist(lapply(lapply(r, igraph::graph_from_incidence_matrix),
                                    igraph::mean_distance)))
    
    out[c, 3] <- out[c, 1] / out[c, 2]
    out[c, 6] <- out[c, 4] / out[c, 5]
    out[c, 7] <- out[c, 3] / out[c, 6]
  }
  out <- cbind(1:ncol(mat), out)
  out <- as.data.frame(out)
  names(out) <- c("Num", "ObsClust", "ExpClust", "ClustRat",
                  "ObsPath", "ExpPath", "PathRat", "SmallWorld")
  out
}

