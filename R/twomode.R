#' Two-mode lattice
#'
#' This function allows you to express your love of lattices.
#' @param m A matrix
#' @keywords two-mode
#' @family two-mode
#' @export
#' @examples
#' twomode_lattice(matrix)
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
#' @param mat A matrix
#' @keywords two-mode
#' @family two-mode
#' @export
#' @examples
#' twomode_clustering(matrix)
twomode_clustering <- function(mat){
  c <- ncol(mat)
  indegrees <- colSums(mat)
  twopaths <- crossprod(mat)
  diag(twopaths) <- 0
  cycle4 <- sum(twopaths * (twopaths-1)) / 
    (sum(twopaths * (twopaths-1)) + sum(twopaths * 
                                          (matrix(indegrees,c,c) - twopaths)))
  if(is.nan(cycle4)) cycle4 <- 1
  return(cycle4)
}

# The following functions were previously named "BBCentralization" and "BDCentralization"
# from a BBCentralization.R script.

#' Two-mode dominance
#'
#' This function allows you to calculate how (degree) centralized a two-mode graph is.
#' @param mat An affiliation or incidence matrix. For centralization around rows, simply transpose the matrix first (\code{t()})
#' @param attr Optionally, an attribute vector.
#' @keywords two-mode
#' @family two-mode
#' @export
#' @examples
#' twomode_dominance(mat)
#' twomode_dominance(mat, attr = gdp2010)
twomode_dominance <- function(mat, attr = NULL){
  
  # Get dimensions
  n <- nrow(mat)
  m <- ncol(mat)
  
  # If attribute absent, use 1s
  if (is.null(attr)) attr <- rep(1, n)

  # Get distributions
  msum <- colSums(mat*attr, na.rm = T)
  
  if(m > 1){
    out <- sum(max(msum)-msum) / (sum(attr)*(m-1))
  } else {
    out <- msum / sum(attr)
  }
  
  return(out)
}

#' Two-mode degree centralization
#'
#' This function allows you to calculate how (degree) centralized a two-mode graph is.
#' @param graph An igraph graph
#' @references Borgatti, Stephen P, and Daniel S Halgin. 2011. ``Analyzing Affiliation Networks." In The SAGE Handbook of Social Network Analysis, edited by John Scott and Peter J Carrington, 417–33. London, UK: Sage.
#' @keywords two-mode
#' @export
#' @examples
#' twomode_centralization_degree(graph)
twomode_centralization_degree <- function(graph){
  require(igraph)
  nodeset <- names(which(igraph::degree(graph)==max(igraph::degree(graph)))) %in%
    V(graph)$name[V(graph)$type==T]
  m <- length(which(V(graph)$type==T))
  n <- length(which(V(graph)$type!=T))

  sum(max(igraph::degree(graph)[which(V(graph)$type==nodeset)], na.rm=T)-
        igraph::degree(graph)[which(V(graph)$type==nodeset)])/
    ((n-1)*(m-1))
}

#' Two-mode betweenness centralization
#'
#' This function allows you to calculate how (betweenness) centralized a two-mode graph is.
#' @param graph An igraph graph
#' @keywords two-mode
#' @family two-mode
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
#' to two-mode networks.
#' @param mat A matrix
#' @return Constraint scores for each second-mode node
#' @details Note that this function returns constraint scores
#' for each second-mode node by default. To return constraint scores
#' for each first-mode node, please pass the function the transpose of the matrix. 
#' See Ron Burt's work on structural holes for more details.
#' @family two-mode
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

#' @title Two-mode fragmentation
#' @description This function identifies components in a two-mode network.
#' @param mat A matrix
#' @return A list including the number of components in the network,
#' the fragmentation of the network (number of components/number of nodes in the column nodeset),
#' and the component membership of each node in that nodeset.
#' @details Note that this function applies only to one dimension/mode (the columns).
#' Use a transposed matrix to return values for the other dimension/mode.
#' @family two-mode
#' @examples 
#' twomode_fragmentation(mat)
#' @rdname twomode_fragmentation
#' @export 
twomode_fragmentation <- function(mat){
  # components - how many institutional fragments do we have?
  m <- ncol(mat)
  twopaths <- crossprod(mat)
  twopaths[lower.tri(twopaths)] <- 0
  diag(twopaths) <- 1
  connect <- which(twopaths > 0, arr.ind = T)
  for (i in 1:m){
    if (sum(connect==i)*1>2) connect <- connect[-which(connect[,1]==i & connect[,2]==i),]
  }

  memb <- data.frame(node=1:m, comp=NA)
  memb[1,2] <- comp <- 1
  if (nrow(memb)==2 & !is.matrix(connect) & all(connect==c(1,2))) {
    memb[2,2] <- 1
  } else {
    while (anyNA(memb[,2])){
      if(anyNA(memb[connect[connect[, 1] %in% which(memb[, 2] == comp), 2], 2])){
        memb[connect[connect[, 1] %in% which(memb[, 2] == comp), 2], 2] <- comp
      } else {
        memb[which(is.na(memb[,2]))[1],2] <- comp <- max(memb[,2], na.rm = T) + 1
      }
      
    }
  }
  
  return(list(components=max(memb[,2]), 
              fragmentation=max(memb[,2])/m, 
              membership=memb))
}

#' @title Two-mode coherence
#' @description This function calculates coherence for a two-mode network
#' @param mat A matrix
#' @return Average coherence across the components of the network
#' @details Note that this function applies only to one dimension/mode (the columns).
#' Use a transposed matrix to return values for the other dimension/mode.
#' @family two-mode
#' @seealso twomode_fragmentation
#' @examples 
#' twomode_coherence(mat)
#' @rdname twomode_coherence
#' @export 
twomode_coherence <- function(mat){
  
  frag <- twomode_fragmentation(mat)
  
  comp.coh <- vector()
  for (c in 1:frag$components){
    
    if (sum(frag$membership$comp==c) > 1){
      mat.comp <- mat[, frag$membership$node[frag$membership$comp==c]]
      
      if(any(colSums(mat.comp)==nrow(mat.comp))) mat.comp <- rbind(mat.comp,0,1)
      
      comp.coh <- c(comp.coh, mean(cor(mat.comp)[lower.tri(cor(mat.comp))]) )
    } else {
      comp.coh <- c(comp.coh, 1)
    }
  }
  return((mean(comp.coh)+1)/2)
}
