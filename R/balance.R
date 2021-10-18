#' Structural balance
#' @param object a migraph-consistent object
#' @importFrom utils combn
#' @importFrom igraph E get.edges
#' @source Gábor Csárdi: http://r.789695.n4.nabble.com/Social-Network-Analysis-td825041.html
#' @return The proportion of all (balanced or imbalanced) triplets that are balanced
#' @examples
#' graph_balance(to_simplex(ison_marvel_relationships))
#' @export
graph_balance <- function(object) { 
  
  g <- as_igraph(object)
  
  triples <- utils::combn(1:vcount(g)-1, 3) 
  good <- bad <- 0 
  for (t in seq_len(ncol(triples))) { 
    
    tri <- triples[,t] 
    edges <- igraph::E(g) [ tri %--% tri ] 
    if (length(unique(igraph::get.edges(g, edges))) < 3) { next } 
    if (prod(igraph::E(g)[edges]$sign) > 0) { 
      good <- good +1 
    } else { 
      bad <- bad +1 
    } 
  }
  
  good/sum(c(good, bad))
}

