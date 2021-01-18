#' Two-mode degree centralization	
#'	
#' This function allows you to calculate how (degree) centralized a two-mode graph is.	
#' @param graph An igraph graph	
#' @references Borgatti, Stephen P, and Daniel S Halgin. 2011. ``Analyzing Affiliation Networks." In The SAGE Handbook of 	
#' Social Network Analysis, edited by John Scott and Peter J Carrington, 417–33. London, UK: Sage.	
# #' @family two-mode functions	
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
# #' @family two-mode functions	
#' @export	
#' @examples	
#' twomode_centralization_betweeness(graph)	
twomode_centralization_betweeness <- function(graph){	
  require(igraph)	
  nodeset <- names(which(betweenness(graph)==max(betweenness(graph)))) %in% 	
    V(graph)$name[V(graph)$type==T]	
  m <- length(which(V(graph)$type==nodeset))	
  n <- length(which(V(graph)$type!=nodeset))	
  # p <- (m-1)%/%n	
  # r <- (m-1)%%n	
  # s <- (n-1)%/%m	
  # t <- (n-1)%%m	
  # 	
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

#' Two Mode Closeness Centralization	
#'	
#' @param graph 	
#' @references Borgatti, Stephen P., and Martin G. Everett. "Network analysis of 2-mode data." Social networks 19.3 (1997): 243-270.	
#' @return	
#' @export
#' @examples	
twomode_centralization_closeness <- function(graph){	
  require(igraph)	
  nodeset <- names(which(closeness(graph)==max(closeness(graph)))) %in% 	
    V(graph)$name[V(graph)$type==T]	
  m <- length(which(V(graph)$type==nodeset))	
  n <- length(which(V(graph)$type!=nodeset))	
  
  if (m > n) {	
    sum(max(closeness(graph)[which(V(graph)$type==nodeset)], na.rm=T)-	
          closeness(graph)[which(V(graph)$type==nodeset)])/	
      (((n-1)*(m-1)/((2*m)-3)) + ((n-1)*(m-n)/m+n-2))	
  }	
  if (m <= n) {	
    sum(max(closeness(graph)[which(V(graph)$type==nodeset)], na.rm=T)-	
          closeness(graph)[which(V(graph)$type==nodeset)])/	
      ((m-2)*(m-1)/((2*m)-3)) 	
  }	
}	
