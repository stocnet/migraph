#' Clique partitioning algorithms
#' 
#' @description 
#'   These functions create a vector of nodes' memberships in
#'   cliques:
#'   
#'   - `node_roulette()` assigns nodes to maximally diverse groups.
#'   
#' @section Maximally diverse grouping problem: 
#'   This well known computational problem is a NP-hard problem
#'   with a number of relevant applications, 
#'   including the formation of groups of students that have encountered
#'   each other least or least recently.
#'   Essentially, the aim is to return a membership of nodes in cliques
#'   that minimises the sum of their previous (weighted) ties:
#'   
#'   \deqn{\sum_{g=1}^{m} \sum_{i=1}^{n-1} \sum_{j=i+1}^{n} x_{ij} y_{ig} y_{jg}}
#'   
#'   where \eqn{y_{ig} = 1} if node \eqn{i} is in group \eqn{g}, and 0 otherwise.
#'   
#'   \eqn{x_{ij}} is the existing network data.
#'   If this is an empty network, the function will just return cliques.
#'   To run this repeatedly, one can join a clique network of the membership result
#'   with the original network, using this as the network data for the next round.
#' 
#'   A form of the Lai and Hao (2016) iterated maxima search (IMS) is used here.
#'   This performs well for small and moderately sized networks.
#'   It includes both weak and strong perturbations to an initial solution
#'   to ensure that a robust solution from the broader state space is identified.
#'   The user is referred to Lai and Hao (2016) and Lai et al (2021) for more details.
#' @inheritParams cohesion
#' @name cliques
#' @family memberships
NULL

#' @rdname cliques
#' @param num_groups An integer indicating the number of groups desired.
#' @param group_size An integer indicating the desired size of most of the groups.
#'   Note that if the number of nodes is not divisible into groups of equal size,
#'   there may be some larger or smaller groups.
#' @param times An integer of the number of search iterations the algorithm should complete.
#'   By default this is the number of nodes in the network multiplied by the number of groups.
#'   This heuristic may be insufficient for small networks and numbers of groups,
#'   and burdensome for large networks and numbers of groups, but can be overwritten.
#'   At every 10th iteration, a stronger perturbation of a number of successive changes,
#'   approximately the number of nodes divided by the number of groups,
#'   will take place irrespective of whether it improves the objective function.
#' @references
#' Lai, Xiangjing, and Jin-Kao Hao. 2016. 
#' “Iterated Maxima Search for the Maximally Diverse Grouping Problem.” 
#' _European Journal of Operational Research_ 254(3):780–800. 
#' \doi{10.1016/j.ejor.2016.05.018}.
#' 
#' Lai, Xiangjing, Jin-Kao Hao, Zhang-Hua Fu, and Dong Yue. 2021. 
#' “Neighborhood Decomposition Based Variable Neighborhood Search and Tabu Search for Maximally Diverse Grouping.” 
#' _European Journal of Operational Research_ 289(3):1067–86. 
#' \doi{10.1016/j.ejor.2020.07.048}.
#' @export
node_roulette <- function(.data, num_groups, group_size, times = NULL){
  if(missing(num_groups) & missing(group_size)){
    stop(paste("Either `num_groups` must indicate number of groups desired",
               "or `group_size` must indicate the desired average size of groups."))
  }
  n <- manynet::network_nodes(.data)
  my_vec <- sample(seq.int(n))
  # Initial partition
  if(!missing(num_groups)){
    out <- cut(seq_along(my_vec), num_groups, labels = FALSE)[my_vec]
  } else {
    out <- ceiling(seq_along(my_vec) / group_size)[my_vec]
  }
  if(is.null(times)) times <- n * max(out)
  # Get fitness
  mat <- manynet::as_matrix(.data)
  fit <- sum(.to_cliques(out) * mat)
  soln <- out
  for(t in seq.int(times)){
    soln <- .weakPerturb(soln)
    new_fit <- sum(.to_cliques(soln) * mat)
    if(new_fit < fit){
      out <- soln
      fit <- new_fit
    } 
    if(t %% 10) soln <- .strongPerturb(soln)
  }
  make_node_member(out, .data)
}

.to_cliques <- function(member){
  (member == t(matrix(member, length(member), length(member))))*1
}

.weakPerturb <- function(soln){
  gsizes <- table(soln)
  evens <- all(gsizes == max(gsizes))
  if(evens){
    soln <- .swapMove(soln)
  } else {
    if(stats::runif(1)<0.5) soln <- .swapMove(soln) else 
      soln <- .oneMove(soln)
  }
  soln
}

.swapMove <- function(soln){
  from <- sample(seq.int(length(soln)), 1)
  to <- sample(which(soln != soln[from]), 1)
  soln[c(to,from)] <- soln[c(from,to)]
  soln
}

.oneMove <- function(soln){
  gsizes <- table(soln)
  maxg <- which(gsizes == max(gsizes))
  from <- sample(which(soln %in% maxg), 1)
  soln[from] <- sample(which(gsizes != max(gsizes)), 1)
  soln
}

.strongPerturb <- function(soln, strength = 1){
  times <- ceiling(strength * length(soln)/max(soln))
  for (t in seq.int(times)){
    soln <- .weakPerturb(soln)
  }
  soln
}


