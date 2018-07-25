#' Two-mode lattice
#'
#' This function allows you to express your love of lattices.
#' @param m A matrix
#' @keywords two-mode
#' @export
#' @examples
#' twomode.lattice(matrix)
twomode.lattice <- function(m){
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
#' twomode.clusterings(matrix)
twomode.clustering <- function(m){
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
#' @param graph An igraph graph
#' @keywords two-mode
#' @export
#' @examples
#' twomode.degree.centralization(graph)
twomode.degree.centralization <- function(graph){
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
#' @export
#' @examples
#' twomode.between.centralization(graph)
twomode.between.centralization <- function(graph){
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
