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
                                                           modes = c("raw", "normalized", "within"),
                                                           ...){
  
  mat <- as_matrix(object)
  modes <- match.arg(modes)
  mode <- c(rep(FALSE, nrow(mat)), rep(TRUE, ncol(mat)))
  
  if(modes == "raw"){
    allcent <- c(rowSums(mat), colSums(mat))
    out <- list()
    out$nodes1 <- sum(max(allcent[!mode]) - allcent)/((nrow(mat) + ncol(mat))*ncol(mat) - 2*(ncol(mat)+nrow(mat)-1))
    out$nodes2 <- sum(max(allcent[mode]) - allcent)/((nrow(mat) + ncol(mat))*nrow(mat) - 2*(ncol(mat)+nrow(mat)-1))
  }
  if(modes == "within"){
    out <- list()
    out$nodes1 <- sum(max(rowSums(mat)) - rowSums(mat))/((ncol(mat)-1)*(nrow(mat)-1))
    out$nodes2 <- sum(max(colSums(mat)) - colSums(mat))/((ncol(mat)-1)*(nrow(mat)-1))
  }
  out
}

#' @rdname centralization
#' @family two-mode functions
#' @export
centralisation_closeness <- centralization_closeness <- function(object, 
                                                                 modes = c("normalized", "within"), 
                                                                 ...){
  
  graph <- as_igraph(object)
  modes <- match.arg(modes)
  
  clcent <- centrality_closeness(graph, normalized = TRUE)
  mode <- igraph::V(graph)$type
  mode1 <- length(mode) - sum(mode)
  mode2 <- sum(mode)
  out <- list()
  
  if(modes == "normalized"){
    term1 <- 2*(mode1 - 1) * (mode2 + mode1 - 4)/(3*mode2 + 4*mode1 - 8)
    term2 <- 2*(mode1 - 1) * (mode1 - 2)/(2*mode2 + 3*mode1 - 6)
    term3 <- 2*(mode1 - 1) * (mode2 - mode1 + 1)/(2*mode2 + 3*mode1 - 4)
    out$nodes1 <- sum(max(clcent[!mode]) - clcent) / sum(term1, term2, term3)
    term1 <- 2*(mode2 - 1) * (mode1 + mode2 - 4)/(3*mode1 + 4*mode2 - 8)
    term2 <- 2*(mode2 - 1) * (mode2 - 2)/(2*mode1 + 3*mode2 - 6)
    term3 <- 2*(mode2 - 1) * (mode1 - mode2 + 1)/(2*mode1 + 3*mode2 - 4)
    out$nodes2 <- sum(max(clcent[mode]) - clcent) / sum(term1, term2, term3)
    
    if (mode1 > mode2) {
      term1 <- 2*(mode2 - 1) * (mode2 + mode1 - 2) / (3 * mode2 + 4 * mode1 - 8)
      term2 <- 2*(mode1 - mode2) * (2 * mode2 - 1) / (5 * mode2 + 2 * mode1 - 6)
      term3 <- 2*(mode2 - 1) * (mode1 - 2) / (2 * mode2 + 3 * mode1 - 6)
      term4 <- 2 * (mode2 - 1) / (mode1 + 4 * mode2 - 4)
      out$nodes1 <- sum(max(clcent[!mode]) - clcent) / sum(term1, term2, term3, term4)
    }
    if (mode2 > mode1) {
      term1 <- 2*(mode1 - 1) * (mode1 + mode2 - 2) / (3 * mode1 + 4 * mode2 - 8)
      term2 <- 2*(mode2 - mode1) * (2 * mode1 - 1) / (5 * mode1 + 2 * mode2 - 6)
      term3 <- 2*(mode1 - 1) * (mode2 - 2) / (2 * mode1 + 3 * mode2 - 6)
      term4 <- 2 * (mode1 - 1) / (mode2 + 4 * mode1 - 4)
      out$nodes2 <- sum(max(clcent[mode]) - clcent) / sum(term1, term2, term3, term4)
    }
  }
  if(modes == "within"){
    out$nodes1 <- sum(max(clcent[!mode])-clcent[!mode])/(((mode1 - 2)*(mode1 - 1))/(2 * mode1 - 3))
    out$nodes2 <- sum(max(clcent[mode])-clcent[mode])/(((mode2 - 2)*(mode2 - 1))/(2 * mode2 - 3))
    if(mode1 > mode2){ #28.43
      lhs <- ((mode2 -1)*(mode1 - 2) / (2 * mode1 - 3))
      rhs <- ((mode2 - 1)*(mode1 - mode2) / (mode1 + mode2 -2))
      out$nodes1 <- sum(max(clcent[!mode])-clcent[!mode])/( lhs +  rhs) # 0.2135
    }
    if (mode2 > mode1) {
      lhs <- ((mode1 -1)*(mode2 - 2) / (2 * mode2 - 3))
      rhs <- ((mode1 - 1)*(mode2 - mode1) / (mode2 + mode1 -2))
      out$nodes2 <- sum(max(clcent[mode])-clcent[mode])/( lhs +  rhs)
    }
  }
  out
}

#' @rdname centralization
#' @family two-mode functions
#' @export
centralisation_betweenness <- centralization_betweenness <- function(object, 
                                                                     modes = c("all", "each"), 
                                                                     ...){
  
  graph <- as_igraph(object)
  
  modes <- match.arg(modes)
  
  if(modes == "all"){
    # Whole network centralization
    bnorm <- c(igraph::betweenness(graph)[which(V(graph)$type!=nodeset)]/
                 ((1/2)*(m^2*(s+1)^2+m*(s+1)*(2*t-s-1)-t*(2*s-t+3))),
               igraph::betweenness(graph)[which(V(graph)$type==nodeset)]/
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
    nodeset <- names(which(igraph::betweenness(graph)==max(igraph::betweenness(graph)))) %in% 
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
      sum(max(igraph::betweenness(graph)[which(V(graph)$type==nodeset)], na.rm=T)-
            igraph::betweenness(graph)[which(V(graph)$type==nodeset)])/
        (2*(m-1)^2*(n-1))
    }
    if (m <= n){
      sum(max(igraph::betweenness(graph)[which(V(graph)$type==nodeset)], na.rm=T)-
            igraph::betweenness(graph)[which(V(graph)$type==nodeset)])/
        ((m-1)*((1/2)*n*(n-1)+(1/2)*(m-1)*(m-2)+(m-1)*(n-1)))
    }
    
  } else stop("Mode not recognised")
  
}
