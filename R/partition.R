#' Kernighan-Lin algorithm for graph partitioning
#'
#' @param object 
#' @param n 
#'
#' @return
#' @examples
#'
#' @export
partition_kernighanlin <- function(object, n = 2){
  # assign groups arbitrarily
  n <- igraph::vcount(object)
  group_size <- ifelse(n %% 2 == 0, n/2, (n+1)/2)
  
  # count internal and external costs of each vertex
  g <- as_matrix(object)
  g1 <- g[1:group_size, 1:group_size]
  g2 <- g[group_size+1:n, group_size+1:n]
  intergroup <- g[1:group_size, group_size+1:n]
  
  g2.intcosts <- rowSums(g2)
  g2.extcosts <- rowSums(intergroup)
  
  g1.intcosts <- rowSums(g1)
  g1.extcosts <- colSums(intergroup)
  
  # count edge costs of each vertex
  g1.net <- g1.extcosts - g1.intcosts
  g2.net <- g2.extcosts - g2.intcosts
  
  g1.net <- sort(g1.net, decreasing = TRUE)
  g2.net <- sort(g2.net, decreasing = TRUE)
  
  # swap pairs of vertices (one from each group) that give a positive sum of net edge costs - ?
  
  # extract names of vertices in each group after swaps
}

# assign groups
# convert to matrices
# count ext and int costs - extract into lists
# count net costs for each vertex in the lists
# sort by highest net cost to lowest net cost
# start swapping from highest net costs (within lists)
# using reordered names in list, reorder the matrix
# assign groups, split the matrix
# copy the row names/col names and print out vertices in lists of two communities


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
