#' Kernighan-Lin algorithm for graph partitioning
#'
#' @param object 
#' @param n 
#'
#' @return
#' @examples
#' partition_kernighanlin(to_unsigned(ison_marvel_relationships, keep = "positive"))
#' @export
partition_kernighanlin <- function(object, n = 2){
  # assign groups arbitrarily
  n <- igraph::vcount(object)
  group_size <- ifelse(n %% 2 == 0, n/2, (n+1)/2)
  
  # count internal and external costs of each vertex
  g <- as_matrix(object)
  g1 <- g[1:group_size, 1:group_size]
  g2 <- g[(group_size+1):as.numeric(n), (group_size+1):as.numeric(n)]
  intergroup <- g[1:group_size, (group_size+1):as.numeric(n)]
  
  g2.intcosts <- rowSums(g2)
  g2.extcosts <- rowSums(intergroup)
  
  g1.intcosts <- rowSums(g1)
  g1.extcosts <- colSums(intergroup)
  
  # count edge costs of each vertex
  g1.net <- g1.extcosts - g1.intcosts
  g2.net <- g2.extcosts - g2.intcosts
  
  g1.net <- sort(g1.net, decreasing = TRUE)
  g2.net <- sort(g2.net, decreasing = TRUE)
  
  # swap pairs of vertices (one from each group) that give a positive sum of net edge costs
  if(length(g1.net)!=length(g2.net)) {
    g2.net <- c(g2.net,0)
  } else {g2.net}
  
  sums <- as.integer(unname(g1.net + g2.net))
  # positions in sequence of names at which sum >= 0
  index <- which(sums >= 0 %in% sums)
  g1.newnames <- g1.names <- names(g1.net)
  g2.newnames <- g2.names <- names(g2.net)
  # swap positions 1-8 in g1 and g2
  for (i in index) {
    g1.newnames[i] <- g2.names[i]
    g2.newnames[i] <- g1.names[i]
  }

  # extract names of vertices in each group after swaps
  out <- list("group 1" = paste0(g1.newnames), "group 2" = paste0(g2.newnames))
  out
}

#' Get biconnected subgraphs
#'
#' @param object 
#' @return a list of biconnected components in the original network
#' @examples
#' 
#' @export
get_biconnected <- function(object){
  object <- as_igraph(object)
  out <- igraph::biconnected_components(object)
  out$components
}
