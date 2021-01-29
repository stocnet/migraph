#' Centralization for one- and two-mode networks
#'
#' These functions allows you to calculate how centralized a two-mode graph is.
#' @name centralization
#' @family two-mode functions
#' @param object A matrix, igraph graph, or tidygraph object
#' @param modes If the object is a two-mode network,
#' whether to calculate centralization based on the degree to which the network as
#' a whole is centralized around each of the two nodesets (`modes = "raw"` or `modes = "normalized"`),
#' or whether centralization is calculated within each nodeset (`modes = "within"`).
#' 
#' In other words, for `raw` and `normalized`, the numerator is the sum of differences
#' between the maximum centrality score for the mode against all other centrality scores in the network,
#' of both nodesets, whereas for `within`, the numerator is the sum of differences
#' between the maximum centrality score for the mode against only the centrality scores of the other
#' nodes in that nodeset.
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
                                                           modes = c("raw", "normalized", "within")){
  
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
                                                                 modes = c("normalized", "within")){
  
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
                                                                     modes = c("raw", "within")){
  
  graph <- as_igraph(object)
  modes <- match.arg(modes)
  
  becent <- centrality_betweenness(graph, normalized = FALSE)
  mode <- igraph::V(graph)$type
  mode1 <- length(mode) - sum(mode)
  mode2 <- sum(mode)
  out <- list()
  
  if(modes == "raw"){
    out$nodes1 <- sum(max(becent[!mode])-becent) / ((1/2 * mode2 * (mode2 - 1) + 1/2 * (mode1 - 1)*(mode1 - 2) + (mode1 - 1) * (mode2 - 2))*(mode1 + mode2 - 1)+(mode1 - 1))
    out$nodes2 <- sum(max(becent[mode])-becent) / ((1/2 * mode1 * (mode1 - 1) + 1/2 * (mode2 - 1)*(mode2 - 2) + (mode2 - 1) * (mode1 - 2))*(mode2 + mode1 - 1)+(mode2 - 1))
    if (mode1 > mode2){
      out$nodes1 <- sum(max(becent[!mode])-becent) / (2 * (mode1 - 1) * (mode2 - 1) * (mode1 + mode2 - 1) - (mode2 - 1) * (mode1 + mode2 - 2) - 1/2 * (mode1 - mode2) * (mode1 + 3*mode2 - 3))
    }
    if (mode2 > mode1){
      out$nodes2 <- sum(max(becent[mode])-becent) / (2 * (mode2 - 1) * (mode1 - 1) * (mode2 + mode1 - 1) - (mode1 - 1) * (mode2 + mode1 - 2) - 1/2 * (mode2 - mode1) * (mode2 + 3*mode1 - 3))
    }
  }
  if(modes == "within"){
    out$nodes1 <- sum(max(becent[!mode])-becent[!mode]) / ((mode1 - 1)*(1/2 * mode2 * (mode2 - 1) + 1/2 * (mode1 - 1) * (mode1 - 2) + (mode1 - 1) * (mode2 - 1)))
    out$nodes2 <- sum(max(becent[mode])-becent[mode]) / ((mode2 - 1)*(1/2 * mode1 * (mode1 - 1) + 1/2 * (mode2 - 1) * (mode2 - 2) + (mode2 - 1) * (mode1 - 1)))
    if (mode1 > mode2){
      out$nodes1 <- sum(max(becent[!mode])-becent[!mode]) / (2 * (mode1 - 1)^2 * (mode2 - 1))
    }
    if (mode2 > mode1){
      out$nodes2 <- sum(max(becent[mode])-becent[mode]) / (2 * (mode2 - 1)^2 * (mode1 - 1))
    }
  }
  out
}
