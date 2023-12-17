# Diffusions ####

#' Functions to play games on networks
#' @inheritParams cohesion
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
#' @param transmissibility The transmission rate probability,
#'   \eqn{\beta}.
#'   By default 1, which means any node for which the threshold is met
#'   or exceeded will become infected.
#'   Anything lower means a correspondingly lower probability of adoption,
#'   even when the threshold is met or exceeded.
#' @param recovery The probability those who are infected
#'   recover, \eqn{\gamma}.
#'   For example, if infected individuals take, on average, 
#'   four days to recover, then \eqn{\gamma = 0.25}.
#'   By default 0, which means there is no recovery (i.e. an SI model).
#'   Anything higher results in an SIR model.
#' @param latency The inverse probability those who have been exposed
#'   become infectious (infected), \eqn{\sigma} or \eqn{\kappa}.
#'   For example, if exposed individuals take, on average, 
#'   four days to become infectious, then \eqn{\sigma = 0.75} (1/1-0.75 = 1/0.25 = 4).
#'   By default 0, which means those exposed become immediately infectious (i.e. an SI model).
#'   Anything higher results in e.g. a SEI model.
#' @param waning The probability those who are recovered 
#'   become susceptible again, \eqn{\xi}.
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
#' @family diffusion
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
  
  infected <- seeds # seeds are initial infected
  latent <- NULL # latent compartment starts empty
  t = 0 # starting at 0
  # initialise events table
  events <- data.frame(t = t, nodes = seeds, event = "I", exposure = NA)
  # initialise report table
  report <- data.frame(t = t,
                       n = n,
                       S = n - (length(latent) + length(infected) + length(recovered)),
                       s = sum(node_is_exposed(.data, infected)),
                       E = length(latent),
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
    # contacts <- unlist(sapply(igraph::neighborhood(.data, nodes = infected),
    #                          function(x) setdiff(x, infected)))
    exposed <- node_is_exposed(.data, infected)
    # count exposures for each node:
    # tabcontact <- table(contacts)
    exposure <- node_exposure(.data, infected)
    # identify those nodes who are exposed at or above their threshold
    # newinf <- as.numeric(names(which(tabcontact >= thresholds[as.numeric(names(tabcontact))])))
    open_to_it <- which(exposure >= thresholds)
    newinf <- open_to_it[stats::rbinom(length(open_to_it), 1, transmissibility)==1]
    if(!is.null(recovery) & length(recovered)>0) 
      newinf <- setdiff(newinf, recovered) # recovered can't be reinfected
    if(!is.null(latent) & length(latent)>0) 
      newinf <- setdiff(newinf, latent) # latent already infected
    if(is.infinite(steps) & length(newinf)==0 & length(latent)==0) break # if no new infections we can stop
    
    # new list of infected 
    latent <- c(latent, newinf)
    infectious <- latent[stats::rbinom(length(latent), 1, latency)==0]
    latent <- setdiff(latent, infectious)
    newinf <- setdiff(newinf, infectious)
    infected <- c(infected, infectious)
    # tick time
    t <- t+1
    
    # Update events table ####
    # record new infections
    if(!is.null(infectious) & length(infectious)>0)
      events <- rbind(events, 
                    data.frame(t = t, nodes = infectious, event = "I", 
                               exposure = exposure[infectious]))
    # record exposures
    if(!is.null(newinf) & length(newinf)>0)
      events <- rbind(events,
                      data.frame(t = t, nodes = newinf, event = "E", 
                                 exposure = exposure[newinf]))
    # record recoveries
    if(!is.null(recovers) & length(recovers)>0)
      events <- rbind(events,
                      data.frame(t = t, nodes = recovers, event = "R", exposure = NA))
    # record wanings
    if(!is.null(waned) & length(waned)>0)
      events <- rbind(events,
                      data.frame(t = t, nodes = waned, event = "S", exposure = NA))
    # Update report table ####
    report <- rbind(report,
                    data.frame(t = t,
                               n = n,
                         S = n - (length(latent) + length(infected) + length(recovered)),
                         s = sum(exposed),
                         E = length(latent),
                         I_new = length(infectious),
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
#'   "optimising" will move the node to coordinates that are most homophilous,
#'   and "minimising" distance will move the node to the next nearest unoccupied coordinates.
#' @examples 
#'   startValues <- rbinom(100,1,prob = 0.5)
#'   startValues[sample(seq_len(100), round(100*0.2))] <- NA
#'   latticeEg <- manynet::create_lattice(100)
#'   latticeEg <- manynet::add_node_attribute(latticeEg, "startValues", startValues)
#'   latticeEg
#'   play_segregation(latticeEg, "startValues", 0.5)
#'   # manynet::autographr(latticeEg, node_color = "startValues", node_size = 5) + 
#'   # manynet::autographr(play_segregation(latticeEg, "startValues", 0.2), 
#'   #                     node_color = "startValues", node_size = 5)
#' @export
play_segregation <- function(.data, 
                             attribute,
                             heterophily = 0,
                             who_moves = c("ordered","random","most_dissatisfied"),
                             choice_function = c("satisficing","optimising", "minimising"),
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
                      satisficing = unoccupied[sample(which(options <= heterophily[unoccupied]), 1)],
                      optimising = unoccupied[which.min(options)[1]],
                      minimising = unoccupied[which.min(igraph::distances(temp, 
                                                                          igraph::V(temp)[dissatisfied], 
                                                                          igraph::V(temp)[unoccupied]))])
    if(is.na(move_to)) next
    print(paste("Moving node", dissatisfied, "to node", move_to))
    temp <- manynet::add_node_attribute(temp, attribute, 
                               swtch(current, dissatisfied, move_to))
    moved <- c(dissatisfied, moved)
  }
  temp
}

#' @describeIn play Coerces a table of diffusion events into
#'   a `diff_model` object similar to the output of `play_diffusion()`
#' @param events A table (data frame or tibble) of diffusion events
#'   with columns `t` indicating the time (typically an integer) of the event, 
#'   `nodes` indicating the number or name of the node involved in the event,
#'   and `event`, which can take on the values "I" for an infection event,
#'   "E" for an exposure event, or "R" for a recovery event.
#' @returns 
#'   `as_diffusion()` and `play_diffusion()` return a 'diff_model' object
#'   that contains two different tibbles (tables) --
#'   a table of diffusion events and 
#'   a table of the number of nodes in each relevant component (S, E, I, or R) --
#'   as well as a copy of the network upon which the diffusion ran.
#'   By default, a compact version of the component table is printed
#'   (to print all the changes at each time point, use `print(..., verbose = T)`).
#'   To retrieve the diffusion events table, use `summary(...)`.
#' @importFrom dplyr tibble
#' @examples
#'   # How to create a diff_model object from (basic) observed data
#'   events <- data.frame(t = c(0,1,1,2,3), nodes = c(1,2,3,2,4), event = c("I","I","I","R","I"))
#'   as_diffusion(events, manynet::create_filled(4))
#' @export
as_diffusion <- function(events, .data) {
  net <- .data
  event <- NULL
  sumchanges <- events |> dplyr::group_by(t) |> 
    dplyr::reframe(I_new = sum(event == "I"),
                   E_new = sum(event == "E"),
                   R_new = sum(event == "R"))
  report <- dplyr::tibble(t = seq_len(max(events$t)) - 1,
                          n = manynet::network_nodes(net)) %>% 
    dplyr::left_join(sumchanges, by = dplyr::join_by(t))
  report[is.na(report)] <- 0
  report$R <- cumsum(report$R_new)
  report$I <- cumsum(report$I_new) - report$R
  report$E <- ifelse(report$E_new == 0 & 
                       cumsum(report$E_new) == max(cumsum(report$E_new)),
                     report$E_new, cumsum(report$E_new))
  report$E <- ifelse(report$R + report$I + report$E > report$n,
                     report$n - (report$R + report$I),
                     report$E)
  report$S <- report$n - report$R - report$I - report$E
  report$s <- vapply(report$t, function(time){
    twin <- dplyr::filter(events, events$t <= time)
    infected <- dplyr::filter(twin, twin$event == "I")$nodes
    recovered <- dplyr::filter(twin, twin$event == "R")$nodes
    infected <- setdiff(infected, recovered)
    expos <- node_is_exposed(net, infected)
    expos[recovered] <- F
    sum(expos)
  }, numeric(1) )
  if (any(report$R + report$I + report$E + report$S != report$n)) {
    stop("Oops, something is wrong")
  }
  report <- dplyr::select(report, dplyr::any_of(c("t", "n", "S", "s", "E", "E_new", "I", "I_new", "R", "R_new")))
  make_diff_model(events, report, .data)
}

# Learning ####

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
