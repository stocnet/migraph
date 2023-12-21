# net_diffusion ####

#' Diffusion metrics for networks
#' @description
#'   These functions allow measurement of various features of
#'   a diffusion process:
#'   
#'   - `network_transmissibility()` measures the average transmissibility observed
#'   in a diffusion simulation, or the number of new infections over
#'   the number of susceptible nodes.
#'   - `network_infection_length()` measures the average number of time steps 
#'   nodes remain infected once they become infected.
#'   - `network_reproduction()` measures the observed reproductive number
#'   in a diffusion simulation as the network's transmissibility over
#'   the network's average infection length.
#'   - `network_immunity()` measures the proportion of nodes that would need
#'   to be protected through vaccination, isolation, or recovery for herd immunity to be reached.
#'   - `network_hazard()` measures the hazard rate or instantaneous probability that
#'   nodes will adopt/become infected at that time
#' @param diff_model A valid network diffusion model,
#'   as created by `as_diffusion()` or `play_diffusion()`.
#' @family measures
#' @family diffusion
#' @name net_diffusion
#' @examples
#'   smeg <- manynet::generate_smallworld(15, 0.025)
#'   smeg_diff <- play_diffusion(smeg, recovery = 0.2)
#'   plot(smeg_diff)
#' @references
#'   Kermack, W. and McKendrick, A., 1927. "A contribution to the mathematical theory of epidemics". 
#'   _Proc. R. Soc. London A_ 115: 700-721.
NULL

#' @rdname net_diffusion 
#' @section Transmissibility: 
#'   `network_transmissibility()` measures how many directly susceptible nodes
#'   each infected node will infect in each time period, on average.
#'   That is:
#'   \deqn{T = \frac{1}{n}\sum_{j=1}^n \frac{i_{j}}{s_{j}}}
#'   where \eqn{i} is the number of new infections in each time period, \eqn{j \in n},
#'   and \eqn{s} is the number of nodes that could have been infected in that time period
#'   (note that \eqn{s \neq S}, or 
#'   the number of nodes that are susceptible in the population).
#'   \eqn{T} can be interpreted as the proportion of susceptible nodes that are
#'   infected at each time period.
#' @examples
#'   # To calculate the average transmissibility for a given diffusion model
#'   network_transmissibility(smeg_diff)
#' @export
network_transmissibility <- function(diff_model){
  out <- diff_model$I_new/diff_model$s
  out <- out[-1]
  out <- out[!is.infinite(out)]
  out <- out[!is.nan(out)]
  make_network_measure(mean(out, na.rm = TRUE),
                       attr(diff_model, "network"))
}

#' @rdname net_diffusion 
#' @section Infection length: 
#'   `network_infection_length()` measures the average number of time steps that
#'   nodes in a network remain infected.
#'   Note that in a diffusion model without recovery, average infection length
#'   will be infinite.
#'   This will also be the case where there is right censoring.
#'   The longer nodes remain infected, the longer they can infect others.
#' @examples
#'   # To calculate the average infection length for a given diffusion model
#'   network_infection_length(smeg_diff)
#' @export
network_infection_length <- function(diff_model){
  make_network_measure(mean(node_infection_length(diff_model), na.rm = TRUE),
                       attr(diff_model, "network"))
}

#' @rdname net_diffusion 
#' @section Reproduction number: 
#'   `network_reproduction()` measures a given diffusion's reproductive number.
#'   Here it is calculated as:
#'   \deqn{R = \min\left(\frac{T}{1/IL}, \bar{k}\right)}
#'   where \eqn{T} is the observed transmissibility in a diffusion
#'   and \eqn{IL} is the observed infection length in a diffusion.
#'   Since \eqn{IL} can be infinite where there is no recovery
#'   or there is right censoring,
#'   and since network structure places an upper limit on how many
#'   nodes each node may further infect (their degree),
#'   this function returns the minimum of \eqn{R_0}
#'   and the network's average degree.
#'   
#'   Interpretation of the reproduction number is oriented around R = 1.
#'   Where \eqn{R > 1}, the 'disease' will 'infect' more and more
#'   nodes in the network.
#'   Where \eqn{R < 1}, the 'disease' will not sustain itself and eventually
#'   die out.
#'   Where \eqn{R = 1}, the 'disease' will continue as endemic,
#'   if conditions allow.
#' @examples
#'   # To calculate the reproduction number for a given diffusion model
#'   network_reproduction(smeg_diff)
#' @export
network_reproduction <- function(diff_model){
  net <- attr(diff_model, "network")
  out <- network_transmissibility(diff_model)/
    (1/network_infection_length(diff_model))
  out <- min(out, mean(node_degree(net, normalized = FALSE)))
  make_network_measure(out, net)
}

#' @rdname net_diffusion 
#' @section Herd immunity: 
#'   `network_immunity()` estimates the proportion of a network
#'   that need to be protected from infection for herd immunity
#'   to be achieved.
#'   This is known as the Herd Immunity Threshold or HIT:
#'   \deqn{1 - \frac{1}{R}}
#'   where \eqn{R} is the reproduction number from `network_reproduction()`.
#'   The HIT indicates the threshold at which
#'   the reduction of susceptible members of the network means
#'   that infections will no longer keep increasing.
#'   Note that there may still be more infections after this threshold has been reached,
#'   but there should be fewer and fewer.
#'   These excess infections are called the _overshoot_.
#'   This function does _not_ take into account the structure
#'   of the network, instead using the average degree.
#'   
#'   Interpretation is quite straightforward.
#'   A HIT or immunity score of 0.75 would mean that 75% of the nodes in the network
#'   would need to be vaccinated or otherwise protected to achieve herd immunity.
#'   To identify how many nodes this would be, multiply this proportion with the number
#'   of nodes in the network. 
#' @examples
#'   # Calculating the proportion required to achieve herd immunity
#'   network_immunity(smeg_diff)
#'   # To find the number of nodes to be vaccinated
#'   ceiling(network_immunity(smeg_diff) * manynet::network_nodes(smeg))
#' @export
network_immunity <- function(diff_model){
  net <- attr(diff_model, "network")
  out <- 1 - 1/network_reproduction(diff_model)
  make_network_measure(out, net)
}

#' @rdname net_diffusion
#' @section Hazard rate: 
#' The hazard rate is the instantaneous probability of adoption/infection at each time point (Allison 1984).
#' In survival analysis, hazard rate is formally defined as:
#'
#' \deqn{%
#' \lambda(t)=\lim_{h\to +0}\frac{F(t+h)-F(t)}{h}\frac{1}{1-F(t)} %
#' }{%
#' \lambda(t-1)= lim (t -> +0) [F(t+h)-F(t)]/h * 1/[1-F(t)] %
#' }
#'
#' By approximating \eqn{h=1}, we can rewrite the equation as
#'
#' \deqn{%
#' \lambda(t)=\frac{F(t+1)-F(t)}{1-F(t)} %
#' }{%
#' \lambda(t-1)= [F(t+1)-F(t)]/[1-F(t)] %
#' }
#'
#' If we estimate \eqn{F(t)}, 
#' the probability of not having adopted the innovation in time \eqn{t}, 
#' from the proportion of adopters in that time, 
#' such that \eqn{F(t) \sim q_t/n}{F(t) ~ q(t)/n}, we now have (ultimately for \eqn{t>1}):
#'
#' \deqn{%
#' \lambda(t)=\frac{q_{t+1}/n-q_t/n}{1-q_t/n} = \frac{q_{t+1} - q_t}{n - q_t} = \frac{q_t - q_{t-1}}{n - q_{t-1}} %
#' }{%
#' \lambda(t-1)= [q(t+1)/n-q(t)/n]/[1-q(t)/n] = [q(t+1) - q(t)]/[n - q(t)] = [q(t) - q(t-1)]/[n - q(t-1)] %
#' }
#' 
#' where \eqn{q_i}{q(i)} is the number of adopters in time \eqn{t}, 
#' and \eqn{n} is the number of vertices in the graph.
#'
#' The shape of the hazard rate indicates the pattern of new adopters over time.
#' Rapid diffusion with convex cumulative adoption curves will have 
#' hazard functions that peak early and decay over time. 
#' Slow concave cumulative adoption curves will have 
#' hazard functions that are low early and rise over time.
#' Smooth hazard curves indicate constant adoption whereas 
#' those that oscillate indicate variability in adoption behavior over time.
#' @source `{netdiffuseR}`
#' @references
#' Allison, P. 1984. _Event history analysis regression for longitudinal event data_. 
#' London: Sage Publications.
#'
#' Wooldridge, J. M. 2010. _Econometric Analysis of Cross Section and Panel Data_ (2nd ed.). 
#' Cambridge: MIT Press.
#' @examples
#' # To calculate the hazard rates at each time point
#' network_hazard(play_diffusion(smeg, transmissibility = 0.3))
#' @export
network_hazard <- function(diff_model){
  out <- (diff_model$I - dplyr::lag(diff_model$I)) / 
    (diff_model$n - dplyr::lag(diff_model$I))
  out
}

# node_diffusion ####

#' Diffusion metrics for nodes
#' @description
#'   These functions allow measurement of various features of
#'   a diffusion process:
#'   
#'   - `node_adoption_time()`: Measures the number of time steps until
#'   nodes adopt/become infected
#'   - `node_adopter()`: Classifies membership of nodes into diffusion categories
#'   - `node_thresholds()`: Measures nodes' thresholds from the amount
#'   of exposure they had when they became infected
#'   - `node_infection_length()`: Measures the average length nodes that become
#'   infected remain infected in a compartmental model with recovery
#'   - `node_exposure()`: Measures how many exposures nodes have to 
#'   a given mark
#'   - `node_is_exposed()`: Marks the nodes that are susceptible,
#'   i.e. are in the immediate neighbourhood of given mark vector
#' @inheritParams cohesion
#' @inheritParams net_diffusion
#' @family measures
#' @family diffusion
#' @name node_diffusion
#' @examples
#'   smeg <- manynet::generate_smallworld(15, 0.025)
#'   smeg_diff <- play_diffusion(smeg, recovery = 0.2)
#'   plot(smeg_diff)
#' @references
#'   Valente, Tom W. 1995. _Network models of the diffusion of innovations_
#'   (2nd ed.). Cresskill N.J.: Hampton Press.
NULL

#' @rdname node_diffusion 
#' @section Adoption time: 
#'   `node_adoption_time()` measures the time units it took 
#'   until each node became infected.
#'   Note that an adoption time of 0 indicates that this was a seed node.
#' @examples
#'   # To measure when nodes adopted a diffusion/were infected
#'   (times <- node_adoption_time(smeg_diff))
#' @export
node_adoption_time <- function(diff_model){
  event <- nodes <- NULL
  out <- summary(diff_model) |> dplyr::filter(event == "I") |> 
    dplyr::distinct(nodes, .keep_all = TRUE) |> 
    dplyr::select(nodes,t)
  net <- attr(diff_model, "network")
  if(!manynet::is_labelled(net))
    out <- dplyr::arrange(out, nodes) else if (is.numeric(out$nodes))
      out$nodes <- manynet::node_names(net)[out$nodes]
  out <- stats::setNames(out$t, out$nodes)
  if(length(out) != manynet::network_nodes(net)){
    full <- rep(Inf, manynet::network_nodes(net))
    names(full) <- `if`(manynet::is_labelled(net), 
                        manynet::node_names(net), 
                        as.character(seq_len(manynet::network_nodes(net))))
    full[match(names(out), names(full))] <- out
    out <- `if`(manynet::is_labelled(net), full, unname(full))
  }
  if(!manynet::is_labelled(net)) out <- unname(out)
  make_node_measure(out, net)
}

#' @rdname node_diffusion
#' @section Adopter class: 
#'   `node_adopter()` classifies the nodes involved in a diffusion
#'   by where on the distribution of adopters they fell.
#'   Valente (1995) defines five memberships:
#'   
#'   - _Early adopter_: those with an adoption time less than 
#'   the average adoption time minus one standard deviation of adoptions times
#'   - _Early majority_: those with an adoption time between
#'   the average adoption time and 
#'   the average adoption time minus one standard deviation of adoptions times
#'   - _Late majority_: those with an adoption time between
#'   the average adoption time and 
#'   the average adoption time plus one standard deviation of adoptions times
#'   - _Laggard_: those with an adoption time greater than 
#'   the average adoption time plus one standard deviation of adoptions times
#'   - _Non-adopter_: those without an adoption time,
#'   i.e. never adopted
#' @examples
#'   # To classify nodes by their position in the adoption curve
#'   (adopts <- node_adopter(smeg_diff))
#'   summary(adopts)
#'   summary(times, membership = adopts)
#' @export
node_adopter <- function(diff_model){
  toa <- node_adoption_time(diff_model)
  toa[is.infinite(toa)] <- NA
  avg <- mean(toa, na.rm = TRUE)
  sdv <- stats::sd(toa, na.rm = TRUE)
  out <- ifelse(toa < (avg - sdv) | toa == 0, "Early Adopter", 
                ifelse(toa > (avg + sdv), "Laggard",
                       ifelse((avg - sdv) < toa & toa <= avg, "Early Majority", 
                              ifelse(avg < toa & toa <= avg + sdv, "Late Majority", 
                                     "Non-Adopter"))))
  out[is.na(out)] <- "Non-Adopter"
  make_node_member(out, attr(diff_model, "network"))
}

#' @rdname node_diffusion 
#' @section Thresholds:
#'   `node_thresholds()` infers nodes' thresholds based on how much
#'   exposure they had when they were infected.
#'   This inference is of course imperfect,
#'   especially where there is a sudden increase in exposure,
#'   but it can be used heuristically.
#' @examples
#'   # To infer nodes' thresholds
#'   node_thresholds(smeg_diff)
#' @export
node_thresholds <- function(diff_model){
  event <- nodes <- NULL
  exposure <- NULL
  out <- summary(diff_model)
  net <- attr(diff_model, "network")
  if(!"exposure" %in% names(out)){
    out[,'exposure'] <- NA_integer_
    for(v in unique(out$t)){
      out$exposure[out$t == v] <- node_exposure(diff_model, time = v)[out$nodes[out$t == v]]
    }
  }
  if(any(out$event == "E")) 
    out <- out |> dplyr::filter(event == "E") else 
      out <- out |> dplyr::filter(event == "I")
  out <- out |> dplyr::distinct(nodes, .keep_all = TRUE) |> 
    dplyr::select(nodes, exposure)
  out <- stats::setNames(out$exposure, out$nodes)
  if(length(out) != manynet::network_nodes(net)){
    full <- stats::setNames(rep(Inf, manynet::network_nodes(net)), 
                     manynet::node_names(net))
    full[match(names(out), names(full))] <- out
    out <- full
  }
  make_node_measure(out, net)
}

#' @rdname node_diffusion 
#' @section Infection length:
#'   `node_infection_length()` measures the average length of time that nodes 
#'   that become infected remain infected in a compartmental model with recovery.
#'   Infections that are not concluded by the end of the study period are
#'   calculated as infinite.
#' @examples
#'   # To measure how long each node remains infected for
#'   node_infection_length(smeg_diff)
#' @export
node_infection_length <- function(diff_model){
  nodes <- NULL
  events <- attr(diff_model, "events")
  out <- vapply(seq_len(diff_model$n[1]), 
         function(x) ifelse("I" %in% dplyr::filter(events, nodes == x)$event,
                          ifelse("R" %in% dplyr::filter(events, nodes == x)$event,
                               mean(diff(dplyr::filter(events, nodes == x)$t)),
                               Inf),
                            NA),
         FUN.VALUE = numeric(1))
  make_node_measure(out, attr(diff_model, "network"))
}

#' @rdname node_diffusion
#' @param mark A valid 'node_mark' object or
#'   logical vector (TRUE/FALSE) of length equal to 
#'   the number of nodes in the network.
#' @param time A time point until which infections/adoptions should be
#'   identified. By default `time = 0`.
#' @section Exposure:
#'   `node_exposure()` calculates the number of infected/adopting nodes
#'   to which each susceptible node is exposed.
#'   It usually expects network data and 
#'   an index or mark (TRUE/FALSE) vector of those nodes which are currently infected,
#'   but if a diff_model is supplied instead it will return
#'   nodes exposure at \eqn{t = 0}.
#' @examples
#'   # To measure how much exposure nodes have to a given mark
#'   node_exposure(smeg, mark = c(1,3))
#'   node_exposure(smeg_diff)
#' @export
node_exposure <- function(.data, mark, time = 0){
  if(missing(mark) && inherits(.data, "diff_model")){
    mark <- node_is_infected(.data, time = time)
    .data <- attr(.data, "network")
  }
  if(is.logical(mark)) mark <- which(mark)
  contacts <- unlist(lapply(igraph::neighborhood(.data, nodes = mark),
                            function(x) setdiff(x, mark)))
  # count exposures for each node:
  tabcontact <- table(contacts)
  out <- rep(0, manynet::network_nodes(.data))
  out[as.numeric(names(tabcontact))] <- unname(tabcontact)
  make_node_measure(out, .data)
}
