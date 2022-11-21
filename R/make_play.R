#' Functions to play games on networks
#' @inheritParams is
#' @param seeds A valid mark vector the length of the
#'   number of nodes in the network.
#' @param thresholds A numeric vector indicating the thresholds
#'   each node has. By default 1.
#'   If less than 1, the threshold is interpreted as complex,
#'   where the threshold concerns the proportion of contacts.
#' @param recovery A proportion indicating the rate of recovery, 
#'   \eqn{\gamma}.
#'   For example, if infected individuals take, on average, 
#'   four days to recover, then $\gamma = 0.25$.
#'   By default NULL, which means there is no recovery (i.e. an SI model). 
#' @param steps The number of steps forward in the diffusion to play.
#'   By default the number of nodes in the network.
#' @family models
#' @name play
NULL

#' @describeIn play Playing compartmental diffusion on networks.
#' @examples 
#' play_diffusion(generate_smallworld(15, 0.025))
#' play_diffusion(generate_smallworld(15, 0.025), thresholds = 0.4)
#' play_diffusion(generate_smallworld(15, 0.025), recovery = 0.4)
#' @export
play_diffusion <- function(object, 
                           seeds = 1,
                           thresholds = 1,
                           recovery = 0,
                           waning = 0,
                           steps){
  n <- network_nodes(object)
  recovered <- NULL
  if(missing(steps)) steps <- n
  if(length(thresholds)==1) thresholds <- rep(thresholds, n)
  if(all(thresholds <= 1) & !all(thresholds == 1)) 
    thresholds <- thresholds * 
      node_degree(object, normalized = FALSE)
  if(is.logical(seeds)) seeds <- which(seeds)
  
  infected <- seeds
  t = 0
  events <- data.frame(t = t, nodes = seeds, event = "I")
  
  repeat{ # At each time step:
    
    # some who have already recovered may lose their immunity:
    waned <- recovered[rbinom(length(recovered), 1, waning)==1]
    # some may recover:
    recovers <- infected[rbinom(length(infected), 1, recovery)==1]
    # the recovered are no longer infected
    recovered <- c(recovered, recovers)
    infected <- setdiff(infected, recovered)
    # those for whom immunity has waned are no longer immune
    recovered <- setdiff(recovered, waned)
    
    # at main infection stage, get currently exposed to infection:
    exposed <- unlist(sapply(igraph::neighborhood(object, nodes = infected),
                             function(x) setdiff(x, infected)))
    # count exposures for each node:
    tabexp <- table(exposed)
    # identify those nodes who are exposed at or above their threshold
    new <- as.numeric(names(which(tabexp >= thresholds[as.numeric(names(tabexp))])))
    if(!is.null(recovery) & length(recovered)>0) 
      new <- setdiff(new, recovered) # recovered can't be reinfected
    if(length(new)==0) break # if no new infections we can stop

    # new list of infected 
    infected <- c(infected, new)
    # tick time
    t <- t+1
    # record new infections
    events <- rbind(events, 
                    data.frame(t = t, nodes = new, event = "I"))
    # record recoveries
    if(!is.null(recovers) & length(recovers)>0)
      events <- rbind(events,
                      data.frame(t = t, nodes = recovers, event = "R"))
    # record wanings
    if(!is.null(waned) & length(waned)>0)
      events <- rbind(events,
                      data.frame(t = t, nodes = waned, event = "S"))
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