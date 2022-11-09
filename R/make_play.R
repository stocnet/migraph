#' Functions to play games on networks
#' @inheritParams is
#' @param seeds A valid mark vector the length of the
#'   number of nodes in the network.
#' @param thresholds A numeric vector indicating the thresholds
#'   each node has. By default 1.
#' @param steps The number of steps forward in the diffusion to play.
#'   By default the number of nodes in the network.
#' @examples 
#' play_diffusion(generate_smallworld(15, 0.025))
#' @export
play_diffusion <- function(object, 
                           seeds = 1:2,
                           thresholds = 1,
                           steps){
  n <- network_nodes(object)
  if(missing(steps)) steps <- n
  if(length(thresholds)==1) thresholds <- rep(thresholds, n)
  
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