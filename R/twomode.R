#' Two-mode lattice
#'
#' This function allows you to express your love of lattices.
#' @param m A matrix
#' @keywords two-mode
#' @export
#' @examples
#' twomode.lattice(matrix)
twomode_lattice <- function(m){
  out <- matrix(c(rep(1, sum(m)), 
                  rep(0, length(m)-sum(m))),
                nrow(m), ncol(m), byrow = T)
  out <- rbind(out,rep(0,ncol(out)))
  out <- matrix(out, nrow(m), ncol(m), byrow = F)
  out
}

#' Two-mode clustering
#'
#' This function allows you to calculate how much two-mode clustering there is.
#' @param m A matrix
#' @keywords two-mode
#' @export
#' @examples
#' twomode.clustering(matrix)
twomode_clustering <- function(m){
  twopaths <- crossprod(m)
  diag(twopaths) <- 0
  indegrees <- colSums(m)
  cycle4 <- sum(twopaths * (twopaths-1)) / 
    (sum(twopaths * (twopaths-1)) + sum(twopaths * 
                                          (matrix(indegrees,c,c) - twopaths)))
  cycle4
}

# The following functions were previously named "BBCentralization" and "BDCentralization"
# from a BBCentralization.R script.

#' Two-mode degree centralization
#'
#' This function allows you to calculate how (degree) centralized a two-mode graph is.
#' @param mat A matrix
#' @keywords two-mode
#' @export
#' @examples
#' twomode_centralization_degree(mat)
twomode_centralization_degree <- function(mat, by = c("both","rows","cols")){
  by <- match.arg(by)
  
  n <- nrow(mat)
  m <- ncol(mat)
  out <- vector()

  if(by %in% c("both","rows")){
    if(n > 1){
      out <- c(out, 
               sum(max(rowSums(mat), na.rm=T)-
            rowSums(mat)) / (m*(n-1)) )
    } else {
      out <- c(out, 
               rowSums(mat) / m )
    }
  }

  if(by %in% c("both","cols")){
    if(m > 1){
      out <- c(out, 
               sum(max(colSums(mat), na.rm=T)-
            colSums(mat)) / (n*(m-1)) )
    } else {
      out <- c(out, 
               colSums(mat) / n )
    }
  }
  return(out)
}

#' #' Two-mode degree centralization
#' #'
#' #' This function allows you to calculate how (degree) centralized a two-mode graph is.
#' #' @param graph An igraph graph
#' #' @keywords two-mode
#' #' @export
#' #' @examples
#' #' twomode_centralization_degree(graph)
#' twomode_centralization_degree <- function(graph){
#'   require(igraph)
#'   nodeset <- names(which(igraph::degree(graph)==max(igraph::degree(graph)))) %in% 
#'     V(graph)$name[V(graph)$type==T]
#'   m <- length(which(V(graph)$type==T))
#'   n <- length(which(V(graph)$type!=T))
#'   
#'   sum(max(igraph::degree(graph)[which(V(graph)$type==nodeset)], na.rm=T)-
#'         igraph::degree(graph)[which(V(graph)$type==nodeset)])/
#'     ((n-1)*(m-1))
#' }

#' Two-mode betweenness centralization
#'
#' This function allows you to calculate how (betweenness) centralized a two-mode graph is.
#' @param graph An igraph graph
#' @keywords two-mode
#' @export
#' @examples
#' twomode_centralization_between(graph)
twomode_centralization_between <- function(graph){
  require(igraph)
  nodeset <- names(which(betweenness(graph)==max(betweenness(graph)))) %in% 
    V(graph)$name[V(graph)$type==T]
  m <- length(which(V(graph)$type==nodeset))
  n <- length(which(V(graph)$type!=nodeset))
  p <- (m-1)%/%n
  r <- (m-1)%%n
  s <- (n-1)%/%m
  t <- (n-1)%%m
  
  # Event side centralization
  # sum(max(betweenness(graph)[which(V(graph)$type==nodeset)])-
  #       betweenness(graph)[which(V(graph)$type==nodeset)])/
  #   ((m-1)*(n^2*(p+1)^2 + n*(p+1)*(2*r-p-1)-r*(2*p-r+3)))/2
  if (m > n){
    sum(max(betweenness(graph)[which(V(graph)$type==nodeset)], na.rm=T)-
          betweenness(graph)[which(V(graph)$type==nodeset)])/
      (2*(m-1)^2*(n-1))
  }
  if (m <= n){
    sum(max(betweenness(graph)[which(V(graph)$type==nodeset)], na.rm=T)-
          betweenness(graph)[which(V(graph)$type==nodeset)])/
      ((m-1)*((1/2)*n*(n-1)+(1/2)*(m-1)*(m-2)+(m-1)*(n-1)))
  }
  
  # Whole network centralization
  # bnorm <- c(betweenness(graph)[which(V(graph)$type!=nodeset)]/
  #              ((1/2)*(m^2*(s+1)^2+m*(s+1)*(2*t-s-1)-t*(2*s-t+3))),
  #            betweenness(graph)[which(V(graph)$type==nodeset)]/
  #              ((1/2)*(n^2*(p+1)^2+n*(p+1)*(2*r-p-1)-r*(2*p-r+3))))
  # sum(max(bnorm)-bnorm)/
  #   ((m+n-1) - ((p*(n-r)*(2*m+2*n-p-3) + r*(p+1)*(2*m+2*n-p-4))/
  #                (m^2*(s+1)^2+m*(s+1)*(2*t-s-1)-t*(2*s-t+3))))
  
  #   if (m > n){
  #     sum(max(betweenness(graph))-betweenness(graph))/
  #       (2*(m-1)*(n-1)*(m+n-1) - (n-1)*(m+n-2) - (1/2)*(m-n)*(m+3*n-3))
  #   }
  #   if (m <= n){
  #     sum(max(betweenness(graph))-betweenness(graph))/
  #       (((1/2)*n*(n-1)+(1/2)*(m-1)*(m-2)+(m-1)*(n-2)) * ((m+n-1)+(m-1)))
  #   }
}

#' @title Two-mode constraint
#' @description This function extends Ronald Burt's constraint measure
#' to two-mode networks. Note that this function returns constraint scores
#' for each second-mode node by default. To return constraint scores
#' for each first-mode node, please pass the function the transpose of the matrix.
#' of 
#' @param mat A matrix
#' @return Constraint scores for each second-mode node
#' @details See Ron Burt's work on structural holes for more details
#' @examples 
#' twomode_constraint(mat)
#' @rdname twomode_constraint
#' @export 
twomode_constraint <- function(mat){
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
    return(res)
  }

