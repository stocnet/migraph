#' Functions to play games on networks
#' @inheritParams is
#' @param seeds A valid mark vector the length of the
#'   number of nodes in the network.
#' @param thresholds A numeric vector indicating the thresholds
#'   each node has. By default 1.
#'   A single number means a generic threshold;
#'   for thresholds that vary among the population please use a vector
#'   the length of the number of nodes in the network.
#'   If 1 or larger, the threshold is interpreted as a simple count
#'   of the number of contacts/exposures sufficient for infection.
#'   If less than 1, the threshold is interpreted as complex,
#'   where the threshold concerns the proportion of contacts.
#' @param transmissibility A proportion indicating the transmission rate,
#'   \eqn{\beta}.
#'   By default 1, which means any node for which the threshold is met
#'   or exceeded will become infected.
#'   Anything lower means a correspondingly lower probability of adoption,
#'   even when the threshold is met or exceeded.
#' @param recovery A proportion indicating the rate of recovery, 
#'   \eqn{\gamma}.
#'   For example, if infected individuals take, on average, 
#'   four days to recover, then \eqn{\gamma = 0.25}.
#'   By default 0, which means there is no recovery (i.e. an SI model).
#'   Anything higher results in an SIR model.
#' @param latency A proportion indicating the rate at which those exposed 
#'   become infectious (infected), \eqn{\sigma}.
#'   For example, if exposed individuals take, on average, 
#'   four days to become infectious, then \eqn{\sigma = 0.25}.
#'   By default 0, which means those exposed become immediately infectious (i.e. an SI model).
#'   Anything higher results in e.g. a SEI model.
#' @param waning A proportion indicating the rate at which those who are
#'   recovered become susceptible again, \eqn{\xi}.
#'   For example, if recovered individuals take, on average,
#'   four days to lose their immunity, then \eqn{\xi = 0.25}.
#'   By default 0, which means any recovered individuals retain lifelong immunity (i.e. an SIR model).
#'   Anything higher results in e.g. a SIRS model.
#'   \eqn{\xi = 1} would mean there is no period of immunity, e.g. an SIS model.
#' @param immune A logical or numeric vector identifying nodes
#'   that begin the diffusion process as already recovered.
#'   This could be interpreted as those who are vaccinated or equivalent.
#'   Note however that a waning parameter will affect these nodes too.
#'   By default NULL, indicating that no nodes begin immune.
#' @param steps The number of steps forward in the diffusion to play.
#'   By default the number of nodes in the network.
#'   If `steps = Inf` then the diffusion process will continue until
#'   there are no new infections or all nodes are infected.
#' @family models
#' @name play
NULL

#' @describeIn play Playing compartmental diffusion on networks.
#' @examples 
#'   smeg <- manynet::generate_smallworld(15, 0.025)
#'   plot(play_diffusion(smeg))
#'   plot(play_diffusion(smeg, recovery = 0.4))
#' @export
play_diffusion <- function(.data, 
                           seeds = 1,
                           thresholds = 1,
                           transmissibility = 1,
                           latency = 0,
                           recovery = 0,
                           waning = 0,
                           immune = NULL,
                           steps){
  n <- manynet::network_nodes(.data)
  exposed <- NULL
  recovered <- NULL
  if(missing(steps)) steps <- n
  if(length(thresholds)==1) thresholds <- rep(thresholds, n)
  if(all(thresholds <= 1) & !all(thresholds == 1)) 
    thresholds <- thresholds * 
      node_degree(.data, normalized = FALSE)
  if(is.logical(seeds)) seeds <- which(seeds)
  if(!is.null(immune)){
    if(is.logical(immune)) immune <- which(immune)
    recovered <- immune
  }
  
  infected <- seeds
  t = 0
  events <- data.frame(t = t, nodes = seeds, event = "I")
  report <- data.frame(t = t,
                       n = n,
                       S = n - (length(exposed) + length(infected) + length(recovered)),
                       E = length(exposed),
                       I_new = length(seeds),
                       I = length(infected),
                       R = length(recovered))
  
  repeat{ # At each time step:
    
    # some who have already recovered may lose their immunity:
    waned <- recovered[stats::rbinom(length(recovered), 1, waning)==1]
    # some may recover:
    recovers <- infected[stats::rbinom(length(infected), 1, recovery)==1]
    # the recovered are no longer infected
    recovered <- c(recovered, recovers)
    infected <- setdiff(infected, recovered)
    # those for whom immunity has waned are no longer immune
    recovered <- setdiff(recovered, waned)
    
    # at main infection stage, get currently exposed to infection:
    contacts <- unlist(sapply(igraph::neighborhood(.data, nodes = infected),
                             function(x) setdiff(x, infected)))
    # count exposures for each node:
    tabcontact <- table(contacts)
    # identify those nodes who are exposed at or above their threshold
    new <- as.numeric(names(which(tabcontact >= thresholds[as.numeric(names(tabcontact))])))
    new <- new[stats::rbinom(length(new), 1, transmissibility)==1]
    if(!is.null(recovery) & length(recovered)>0) 
      new <- setdiff(new, recovered) # recovered can't be reinfected
    if(!is.null(exposed) & length(exposed)>0) 
      new <- setdiff(new, exposed) # exposed already infected
    if(is.infinite(steps) & length(new)==0 & length(exposed)==0) break # if no new infections we can stop
    exposed <- c(exposed, new)

    # new list of infected 
    infectious <- exposed[stats::rbinom(length(exposed), 1, latency)==0]
    exposed <- setdiff(exposed, infectious)
    infected <- c(infected, infectious)
    # tick time
    t <- t+1
    # record new infections
    if(!is.null(new) & length(new)>0)
      events <- rbind(events, 
                    data.frame(t = t, nodes = new, event = "I"))
    # record recoveries
    if(!is.null(exposed) & length(exposed)>0)
      events <- rbind(events,
                      data.frame(t = t, nodes = exposed, event = "E"))
    # record recoveries
    if(!is.null(recovers) & length(recovers)>0)
      events <- rbind(events,
                      data.frame(t = t, nodes = recovers, event = "R"))
    # record wanings
    if(!is.null(waned) & length(waned)>0)
      events <- rbind(events,
                      data.frame(t = t, nodes = waned, event = "S"))
    report <- rbind(report,
                    data.frame(t = t,
                               n = n,
                         S = n - (length(exposed) + length(infected) + length(recovered)),
                         E = length(exposed),
                         I_new = length(new),
                         I = length(infected),
                         R = length(recovered)))
    if(is.infinite(steps) & length(infected)==n) break
    if(t==steps) break
  }
  make_diff_model(events, report, .data)
}

#' @describeIn play Playing multiple compartmental diffusions on networks.
#' @inheritParams regression
#' @examples 
#'   plot(play_diffusions(smeg, times = 20))
#' @export
play_diffusions <- function(.data,
                            seeds = 1,
                            thresholds = 1,
                            transmissibility = 1,
                            latency = 0,
                            recovery = 0,
                            waning = 0,
                            immune = NULL,
                            steps,
                            times = 5,
                            strategy = "sequential",
                            verbose = FALSE){
  if(missing(steps)) steps <- manynet::network_nodes(.data)
  future::plan(strategy)
  out <- furrr::future_map_dfr(1:times, function(j){
      data.frame(sim = j,
                 play_diffusion(.data, 
                     seeds = seeds, thresholds = thresholds,
                     transmissibility = transmissibility,
                     latency = latency, recovery = recovery, waning = waning,
                     immune = immune, steps = steps))
    }, .progress = verbose, .options = furrr::furrr_options(seed = T))
  make_diffs_model(out, .data)
}

#' @describeIn play Playing DeGroot learning on networks.
#' @param beliefs A vector indicating the probabilities nodes
#'   put on some outcome being 'true'.
#' @param epsilon The maximum difference in beliefs accepted
#'   for convergence to a consensus.
#' @examples 
#'   play_learning(ison_networkers, 
#'       rbinom(manynet::network_nodes(ison_networkers),1,prob = 0.25))
#' @export
play_learning <- function(.data, 
                           beliefs,
                           steps,
                          epsilon = 0.0005){
  n <- manynet::network_nodes(.data)
  if(length(beliefs)!=n) 
    stop("'beliefs' must be a vector the same length as the number of nodes in the network.")
  if(is.logical(beliefs)) beliefs <- beliefs*1
  if(missing(steps)) steps <- n

  t = 0
  out <- matrix(NA,steps+1,length(beliefs))
  out[1,] <- beliefs
  trust_mat <- manynet::as_matrix(.data)/rowSums(manynet::as_matrix(.data))
  
  repeat{
    old_beliefs <- beliefs
    beliefs <- trust_mat %*% beliefs
    if(all(abs(old_beliefs - beliefs) < epsilon)) break
    t = t+1
    out[t+1,] <- beliefs
    if(t==steps) break
  }
  out <- stats::na.omit(out)
  
  make_learn_model(out, .data)
}


#' @describeIn play Playing Schelling segregation on networks.
#' @param attribute A string naming some nodal attribute in the network.
#'   Currently only tested for binary attributes.
#' @param heterophily A score ranging between -1 and 1 as a threshold for 
#'   how heterophilous nodes will accept their neighbours to be.
#'   A single proportion means this threshold is shared by all nodes,
#'   but it can also be a vector the same length of the nodes in the network
#'   for issuing different thresholds to different nodes.
#'   By default this is 0, meaning nodes will be dissatisfied if more than half
#'   of their neighbours differ on the given attribute.
#' @param who_moves One of the following options:
#'   "ordered" (the default) checks each node in turn for whether they are
#'   dissatisfied and there is an available space that they can move to,
#'   "random" will check a node at random, 
#'   and "most_dissatisfied" will check (one of) the most dissatisfied nodes first.
#' @param choice_function One of the following options:
#'   "satisficing" (the default) will move the node to any coordinates that satisfy
#'   their heterophily threshold,
#'   whereas "optimising" will move the node to coordinates that are most homophilous.
#' @examples 
#'   startValues <- rbinom(100,1,prob = 0.5)
#'   startValues[sample(seq_len(100), round(100*0.2))] <- NA
#'   latticeEg <- manynet::create_lattice(100)
#'   latticeEg <- manynet::add_node_attribute(latticeEg, "startValues", startValues)
#'   manynet::autographr(latticeEg, node_color = "startValues", node_size = 5)
#'   manynet::autographr(play_segregation(latticeEg, "startValues", 0.5), 
#'              node_color = "startValues", node_size = 5)
#' @export
play_segregation <- function(.data, 
                             attribute,
                             heterophily = 0,
                             who_moves = c("ordered","random","most_dissatisfied"),
                             choice_function = c("satisficing","optimising"),
                             steps){
  n <- manynet::network_nodes(.data)
  if(missing(steps)) steps <- n
  who_moves <- match.arg(who_moves)
  choice_function <- match.arg(choice_function)
  if(length(heterophily)==1) heterophily <- rep(heterophily, n)
  if(length(heterophily)!=n) stop("Heterophily threshold must be the same length as the number of nodes in the network.")
  swtch <- function(x,i,j) {x[c(i,j)] <- x[c(j,i)]; x} 

  t = 0
  temp <- .data
  moved <- NULL
  while(steps > t){
    t <- t+1
    current <- manynet::node_attribute(temp, attribute)
    heterophily_scores <- node_heterophily(temp, attribute)
    dissatisfied <- which(heterophily_scores > heterophily)
    unoccupied <- which(is.na(current))
    dissatisfied <- setdiff(dissatisfied, unoccupied)
    dissatisfied <- setdiff(dissatisfied, moved)
    if(length(dissatisfied)==0) break
    dissatisfied <- switch(who_moves,
                           ordered = dissatisfied[1],
                           random = sample(dissatisfied, 1),
                           most_dissatisfied = dissatisfied[
                             which(heterophily_scores[dissatisfied] == 
                                     max(heterophily_scores[dissatisfied]))[1]])
    options <- vapply(unoccupied, function(u){
      test <- manynet::add_node_attribute(temp, "test", 
                                 swtch(current, dissatisfied, u))
      node_heterophily(test, "test")[u]
    }, FUN.VALUE = numeric(1))
    if(length(options)==0) next
    move_to <- switch(choice_function,
                      satisficing = unoccupied[which(options <= heterophily)[1]],
                      optimising = unoccupied[which(options == min(options))[1]])
    if(is.na(move_to)) next
    print(paste("Moving node", dissatisfied, "to node", move_to))
    temp <- manynet::add_node_attribute(temp, attribute, 
                               swtch(current, dissatisfied, move_to))
    moved <- c(dissatisfied, moved)
  }
  temp
}
