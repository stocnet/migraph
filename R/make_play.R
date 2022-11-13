#' Functions to play games on networks
#' @inheritParams is
#' @param seeds A valid mark vector the length of the
#'   number of nodes in the network.
#' @param thresholds A numeric vector indicating the thresholds
#'   each node has. By default 1.
#' @param steps The number of steps forward in the diffusion to play.
#'   By default the number of nodes in the network.
#' @family models
#' @name play
NULL

#' @describeIn play Playing compartmental diffusion on networks.
#' @examples 
#' play_diffusion(generate_smallworld(15, 0.025))
#' play_diffusion(generate_smallworld(15, 0.025), thresholds = 0.4)
#' @export
play_diffusion <- function(object, 
                           seeds = 1:2,
                           thresholds = 1,
                           steps){
  n <- network_nodes(object)
  if(missing(steps)) steps <- n
  if(length(thresholds)==1) thresholds <- rep(thresholds, n)
  if(all(thresholds <= 1) & !all(thresholds == 1)) 
    thresholds <- thresholds * 
      node_degree(object, normalized = FALSE)
  if(is.logical(seeds)) seeds <- which(seeds)
  
  infected <- seeds
  t = 0
  events <- data.frame(t = t, nodes = seeds)
  
  repeat{
    exposed <- unlist(sapply(igraph::neighborhood(object, nodes = infected),
                             function(x) setdiff(x, infected)))
    tabexp <- table(exposed)
    new <- as.numeric(names(which(tabexp > thresholds[as.numeric(names(tabexp))])))
    if(length(new)==0) break
    infected <- c(infected, new)
    t <- t+1
    events <- rbind(events, data.frame(t = t, nodes = new))
    if(length(infected)==n) break
    if(t==steps) break
  }
  make_diff_model(events, object)
}

#' @describeIn play Playing DeGroot learning on networks.
#' @examples 
#' play_learning(ison_networkers, 
#'       rbinom(network_nodes(ison_networkers),1,prob = 0.25))
#' @export
play_learning <- function(object, 
                           beliefs,
                           steps,
                          epsilon = 0.0005){
  n <- network_nodes(object)
  if(is.logical(beliefs)) beliefs <- beliefs*1
  if(missing(steps)) steps <- n

  t = 0
  out <- matrix(NA,steps+1,length(beliefs))
  out[1,] <- beliefs
  trust_mat <- as_matrix(object)/rowSums(as_matrix(object))
  
  repeat{
    old_beliefs <- beliefs
    beliefs <- trust_mat %*% beliefs
    if(all(abs(old_beliefs - beliefs) < epsilon)) break
    t = t+1
    out[t+1,] <- beliefs
    if(t==steps) break
  }
  out <- na.omit(out)
  
  make_learn_model(out, object)
}