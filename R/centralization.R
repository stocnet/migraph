#' Centralization for one- and two-mode graphs
#'
#' This function allows you to calculate how (degree) centralized a two-mode graph is.
#' @name centralization
#' @family two-mode functions
#' @param object A matrix, igraph graph, or tidygraph object
#' @param modes Whether to calculate centralization for the graph as a whole (`modes = "all"`),
#' or, if two-mode, for each nodeset (`modes = "each"`).
#' @param ... 
#' @return The centralization score. 
#' If `modes = "each"`, then a named list of two scores will be returned.
#' To return just the score for the first nodeset (rows), 
#' append `$nodes1` to the end of the function call or returned object.
#' To return just the score for the second nodeset (cols), 
#' append `$nodes2` to the end of the function call or returned object.
#' @references Borgatti, Stephen P, and Daniel S Halgin. 2011. ``Analyzing Affiliation Networks." In \emph{The SAGE Handbook of 
#' Social Network Analysis}, edited by John Scott and Peter J Carrington, 417–33. London, UK: Sage.
#' @examples
#' \dontrun{
#' centralization_degree(graph)
#' }
#' @export
centralisation_degree <- centralization_degree <- function(object, 
                                                           modes = c("all", "each"), 
                                                           ...){
  
  graph <- converge_to_igraph(object)
  
  modes <- match.arg(modes)
  
  if(modes == "all"){
    nodeset <- names(which(igraph::degree(graph)==max(igraph::degree(graph)))) %in%
      V(graph)$name[V(graph)$type==T]
    m <- length(which(V(graph)$type==T))
    n <- length(which(V(graph)$type!=T))
    
    out <- sum(max(igraph::degree(graph)[which(V(graph)$type==nodeset)], na.rm=T)-
                 igraph::degree(graph)[which(V(graph)$type==nodeset)])/((n-1)*(m-1))
  } else if (modes == "each"){
    out <- vector()
    out$nodes1 <- "score1"
    out$nodes2 <- "score2"
  } else stop("Mode not recognised")
  
}

#' @rdname centralization
#' @family two-mode functions
#' @export
centralisation_closeness <- centralization_closeness <- function(object, 
                                                                 modes = c("all", "each"), 
                                                                 ...){
  
  graph <- converge_to_igraph(object)
  
  modes <- match.arg(modes)
  
  if(modes == "all"){
    nodeset <- names(which(igraph::closeness(graph)==max(igraph::closeness(graph)))) %in% 
      igraph::V(graph)$name[igraph::V(graph)$type==T]
    m <- length(which(igraph::V(graph)$type==nodeset))
    n <- length(which(igraph::V(graph)$type!=nodeset))
    
    if (m > n) {
      sum(max(igraph::closeness(graph)[which(igraph::V(graph)$type==nodeset)], na.rm=T)-
            igraph::closeness(graph)[which(igraph::V(graph)$type==nodeset)])/
        (((n-1)*(m-1)/((2*m)-3)) + ((n-1)*(m-n)/m+n-2))
    }
    if (m <= n) {
      sum(max(igraph::closeness(graph)[which(igraph::V(graph)$type==nodeset)], na.rm=T)-
            igraph::closeness(graph)[which(igraph::V(graph)$type==nodeset)])/
        ((m-2)*(m-1)/((2*m)-3)) 
    }
  } else if (modes == "each"){
    out <- vector()
    out$nodes1 <- "score1"
    out$nodes2 <- "score2"
  } else stop("Mode not recognised")
  
}

#' @rdname centralization
#' @family two-mode functions
#' @export
centralisation_betweenness <- centralization_betweenness <- function(object, 
                                                                     modes = c("all", "each"), 
                                                                     ...){
  
  graph <- converge_to_igraph(object)
  
  modes <- match.arg(modes)
  
  if(modes == "all"){
    # Whole network centralization
    bnorm <- c(betweenness(graph)[which(V(graph)$type!=nodeset)]/
                 ((1/2)*(m^2*(s+1)^2+m*(s+1)*(2*t-s-1)-t*(2*s-t+3))),
               betweenness(graph)[which(V(graph)$type==nodeset)]/
                 ((1/2)*(n^2*(p+1)^2+n*(p+1)*(2*r-p-1)-r*(2*p-r+3))))
    sum(max(bnorm)-bnorm)/
      ((m+n-1) - ((p*(n-r)*(2*m+2*n-p-3) + r*(p+1)*(2*m+2*n-p-4))/
                    (m^2*(s+1)^2+m*(s+1)*(2*t-s-1)-t*(2*s-t+3))))
    
    #   if (m > n){
    #     sum(max(betweenness(graph))-betweenness(graph))/
    #       (2*(m-1)*(n-1)*(m+n-1) - (n-1)*(m+n-2) - (1/2)*(m-n)*(m+3*n-3))
    #   }
    #   if (m <= n){
    #     sum(max(betweenness(graph))-betweenness(graph))/
    #       (((1/2)*n*(n-1)+(1/2)*(m-1)*(m-2)+(m-1)*(n-2)) * ((m+n-1)+(m-1)))
    #   }
    
  } else if (modes == "each"){
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
    
  } else stop("Mode not recognised")
  
}
