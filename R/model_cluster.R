#' Methods for equivalence clustering
#' @name cluster
#' @inheritParams equivalence
NULL

#' @describeIn cluster Returns a hierarchical clustering object
#'   created by `stats::hclust()`
#' @export
cluster_hierarchical <- function(census, distance){
  correlations <- cor(t(census))
  dissimilarity <- 1 - correlations
  distances <- stats::dist(dissimilarity, method = distance)
  hc <- stats::hclust(distances)
  hc$distances <- distances
  hc
}

# cluster_concor(ison_adolescents)
# cluster_concor(ison_southern_women)
# https://github.com/bwlewis/hclust_in_R/blob/master/hc.R

#' @describeIn cluster Returns a hierarchical clustering object
#'   created from a convergence of correlations procedure (CONCOR)
#' @references 
#' Breiger, Ronald L., Scott A. Boorman, and Phipps Arabie. 1975.  
#'   "An Algorithm for Clustering Relational Data with Applications to 
#'   Social Network Analysis and Comparison with Multidimensional Scaling". 
#'   _Journal of Mathematical Psychology_, 12: 328-83.
#'   \doi{10.1016/0022-2496(75)90028-0}.
#' @export
cluster_concor <- function(object, census){
  split_cor <- function(m0, cutoff = 1) {
    if (ncol(m0) < 2 | all(stats::cor(m0)==1)) list(m0)
    else {
      mi <- stats::cor(m0)
      while (any(abs(mi) <= cutoff)) {
        mi <- cor(mi)
        cutoff <- cutoff - 0.0001
      }
      group <- mi[, 1] > 0
      list(m0[, group, drop = FALSE], 
           m0[, !group, drop = FALSE])
    }
  }
  p_list <- list(t(census))
  p_group <- list()
  i <- 1
  while(!all(vapply(p_list, function(x) ncol(x)==1, logical(1)))){
    p_list <- unlist(lapply(p_list,
                            function(y) split_cor(y)),
                     recursive = FALSE)
    p_group[[i]] <- lapply(p_list, function(z) colnames(z))
    if(i > 2 && length(p_group[[i]]) == length(p_group[[i-1]])) break
    i <- i+1
  }
  
  merges <- sapply(rev(1:(i-1)), 
                   function(p) lapply(p_group[[p]], 
                                      function(s){
                                        g <- match(s, node_names(object))
                                        if(length(g)==1) c(g, 0, p) else 
                                          if(length(g)==2) c(g, p) else
                                            c(t(cbind(t(utils::combn(g, 2)), p)))
                                      } ))
  merges <- c(merges, 
              list(c(t(cbind(t(utils::combn(seq_len(graph_nodes(object)), 2)), 0)))))
  merged <- matrix(unlist(merges), ncol = 3, byrow = TRUE)
  merged <- merged[!duplicated(merged[,1:2]),]
  merged[,3] <- abs(merged[,3] - max(merged[,3]))
  
  distances <- as_matrix(as_igraph(as.data.frame(merged)))
  distances <- distances + t(distances)
  distances <- distances[-which(rownames(distances)==0),-which(colnames(distances)==0)]
  rownames(distances) <- colnames(distances) <- node_names(object)[as.numeric(colnames(distances))]
  hc <- hclust(d = as.dist(distances))
  hc$method <- "concor"
  hc$distances <- distances
  hc  
}
