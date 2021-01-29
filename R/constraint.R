#' Constraint for one- and two-mode networks
#' 
#' This function calculates Ronald Burt's constraint measure for 
#' both one-mode and two-mode networks.
#' @param object A matrix, igraph graph, or tidygraph object.
#' @param nodes The vertices for which the constraint will be calculated. 
#' Defaults to all vertices.
#' @param weights The weights of the edges. 
#' If this is NULL and there is a weight edge attribute this is used. 
#' If there is no such edge attribute all edges will have the same weight.
#' @return Constraint scores for each node set
#' @details Note that this function returns constraint scores
#' for each second-mode node by default. To return constraint scores
#' for each first-mode node, please pass the function the transpose of the matrix. 
#' See Ron Burt's work on structural holes for more details.
#' @family two-mode functions
#' @examples
#' constraint(southern_women)
#' @export 
constraint <- function(object, nodes = V(object), weights = NULL){
  
  object <- as_igraph(object)
  
  if(!is_bipartite(object)){
    res <- igraph::constraint(object, nodes = nodes, weights = weights)
  } else {
    mat <- as_matrix(object)
    inst <- colnames(mat)
    rowp <- mat * matrix(1/rowSums(mat), nrow(mat), ncol(mat))
    colp <- mat * matrix(1/colSums(mat), nrow(mat), ncol(mat), byrow = T)
    res <- vector()
    for (i in inst){
      ci <- 0
      membs <- names(which(mat[,i]==1))
      for (a in membs){
        pia <- colp[a,i]
        oth <- membs[membs!=a]
        pbj <- 0
        if (length(oth)==1){
          for (j in inst[mat[oth,]>0 & inst!=i]){
            pbj <- sum(pbj, sum(colp[oth,i] * rowp[oth,j] * colp[a,j]))
          }
        } else {
          for (j in inst[colSums(mat[oth,])>0 & inst!=i]){
            pbj <- sum(pbj, sum(colp[oth,i] * rowp[oth,j] * colp[a,j]))
          }
        }
        cia <- (pia + pbj)^2
        ci <- sum(ci, cia)
      }
      res <- rbind(res, c(i,round(ci*100)))
    }
  }
  res
}

