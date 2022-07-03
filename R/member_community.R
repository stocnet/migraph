#' Community graph partitioning algorithms
#' @inheritParams is
#' @name community
#' @family membership
NULL

#' @describeIn community A greedy, iterative, deterministic graph
#'   partitioning algorithm that results in a graph with two 
#'   equally-sized communities
#' @references
#' Kernighan, Brian W., and Shen Lin. 1970.
#' "An efficient heuristic procedure for partitioning graphs."
#' _The Bell System Technical Journal_ 49(2): 291-307.
#' \doi{10.1002/j.1538-7305.1970.tb01770.x}
#' @examples
#' node_kernighanlin(ison_adolescents)
#' node_kernighanlin(ison_southern_women)
#' @export
node_kernighanlin <- function(object){
  # assign groups arbitrarily
  n <- graph_nodes(object)
  group_size <- ifelse(n %% 2 == 0, n/2, (n+1)/2)
  
  # count internal and external costs of each vertex
  g <- as_matrix(to_multilevel(object))
  g1 <- g[1:group_size, 1:group_size]
  g2 <- g[(group_size+1):n, (group_size+1):n]
  intergroup <- g[1:group_size, (group_size+1):n]
  
  g2.intcosts <- rowSums(g2)
  g2.extcosts <- colSums(intergroup)
  
  g1.intcosts <- rowSums(g1)
  g1.extcosts <- rowSums(intergroup)
  
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
  # make swaps based on positions in sequence
  for (i in index) {
    g1.newnames[i] <- g2.names[i]
    g2.newnames[i] <- g1.names[i]
  }
  
  # extract names of vertices in each group after swaps
  out <- ifelse(node_names(object) %in% g1.newnames, 1, 2)
  if(is_labelled(object)) names(out) <- node_names(object)
  make_node_member(out, object)
}

