# net_diffusion ####

#' Diffusion metrics for networks
#' @description
#'   These functions allow measurement of various features of
#'   a diffusion process:
#'   
#'   - `network_transmissibility()`: Measures the average transmissibility observed
#'   in a diffusion simulation, or the number of new infections over
#'   the number of susceptible nodes
#'   - `network_infection_length()`: Measures the average length nodes remain
#'   infected in a compartmental model with recovery for the network as a whole
#'   - `network_reproduction()`: Measures the observed reproductive number
#'   in a diffusion simulation as the network's transmissibility over
#'   the network's average infection length
#' @param diff_model A valid network diffusion model,
#'   as created by `as_diffusion()` or `play_diffusion()`.
#' @family measures
#' @name net_diffusion
#' @examples
#'   smeg <- manynet::generate_smallworld(15, 0.025)
#'   smeg_diff <- play_diffusion(smeg, recovery = 0.2)
#'   plot(smeg_diff)
#'   autographs(smeg_diff)
#'   # autographd(smeg_diff)
#'   summary(node_adoption_time(smeg_diff), membership = adopts)
#'   summary(node_thresholds(smeg_diff), membership = adopts)
#'   summary(node_infection_length(smeg_diff))
#'   network_transmissibility(smeg_diff)
#' @references
#'   Kermack, W. and McKendrick, A., 1927. "A contribution to the mathematical theory of epidemics". 
#'   _Proc. R. Soc. London A_ 115: 700-721.
NULL

#' @rdname net_diffusion 
#' @section Transmissibility: 
#'   `network_transmissibility()` measures the average proportion of new infections
#'   over those that are directly susceptible or exposed to infection during
#'   a diffusion's infectious period (the period during which there are new infections).
#'   That is:
#'   \deqn{T = \frac{\sum_i\frac{I_new}{s}}{t}}
#'   where \eqn{I_new} is the number of new infections in each time period, \eqn{i \in t},
#'   \eqn{s} is the number of nodes that could have been infected in that time period
#'   (note that this is not the same as \eqn{S}, 
#'   the number of nodes that are still susceptible in the population).
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
#'   `network_infection_length()` measures the average length of time that
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

#' @describeIn diffusion Measures nodes' time of adoption/infection

# node_diffusion ####

#' Diffusion metrics for nodes
#' @description
#'   These functions allow measurement of various features of
#'   a diffusion process:
#'   
#'   - `node_adoption_time()`: Measures nodes' time of adoption/infection
#'   - `node_adopter()`: Classifies membership of nodes into diffusion categories
#'   - `node_thresholds()`: Measures nodes' thresholds from the amount
#'   of exposure they had when they became infected
#'   - `node_infection_length()`: Measures the average length nodes that become
#'   infected remain infected in a compartmental model with recovery
#'   - `node_exposure()`: Measures how many exposures nodes have to 
#'   a given mark
#'   - `node_is_exposed()`: Marks the nodes that are susceptible,
#'   i.e. are in the immediate neighbourhood of given mark vector
#' @inheritParams net_diffusion
#' @family measures
#' @name node_diffusion
#' @examples
#'   smeg <- manynet::generate_smallworld(15, 0.025)
#'   smeg_diff <- play_diffusion(smeg, recovery = 0.2)
#'   plot(smeg_diff)
#'   autographs(smeg_diff)
#'   # autographd(smeg_diff)
#'   (adopts <- node_adopter(smeg_diff))
#'   summary(adopts)
#'   summary(node_adoption_time(smeg_diff), membership = adopts)
#'   summary(node_thresholds(smeg_diff), membership = adopts)
#'   summary(node_infection_length(smeg_diff))
#'   network_infection_length(smeg_diff)
#'   network_transmissibility(smeg_diff)
#'   network_reproduction(smeg_diff)
#' @references
#'   Valente, Tom W. (1995). _Network models of the diffusion of innovations_
#'   (2nd ed.). Cresskill N.J.: Hampton Press.
NULL

#' @rdname node_diffusion 
#' @examples
#' # example code
#'   smeg <- manynet::generate_smallworld(15, 0.025)
#'   smeg_diff <- play_diffusion(smeg, recovery = 0.2)
#'   (adopts <- node_adopter(smeg_diff))
#'   summary(adopts)
#' @export
node_adoption_time <- function(diff_model){
  event <- nodes <- NULL
  out <- summary(diff_model) |> dplyr::filter(event == "I") |> 
    dplyr::distinct(nodes, .keep_all = TRUE) |> 
    dplyr::select(t) |> c() |> unname() |> unlist()
  make_node_measure(out, attr(diff_model, "network"))
}

#' @rdname node_diffusion 
#' @export
node_adopter <- function(diff_model){
  toa <- node_adoption_time(diff_model)
  avg <- mean(toa)
  sdv <- stats::sd(toa)
  out <- ifelse(toa < (avg - sdv), "Early Adopter", 
         ifelse(toa > (avg + sdv), "Laggard",
                ifelse((avg - sdv) < toa & toa <= avg, "Early Majority", 
                       ifelse(avg < toa & toa <= avg + sdv, "Late Majority", "Non-Adopter"))))
  make_node_member(out, attr(diff_model, "network"))
}

#' @rdname node_diffusion 
#' @export
node_thresholds <- function(diff_model){
  event <- nodes <- NULL
  exposure <- NULL
  out <- summary(diff_model)
  if(any(out$event == "E")) 
    out <- out |> dplyr::filter(event == "E") else 
      out <- out |> dplyr::filter(event == "I")
  out <- out |> dplyr::distinct(nodes, .keep_all = TRUE) |> 
    dplyr::select(exposure) |> c() |> unname() |> unlist()
  make_node_measure(out, attr(diff_model, "network"))
}

#' @rdname node_diffusion 
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
#' @export
node_exposure <- function(.data, mark){
  if(is.logical(mark)) mark <- which(mark)
  contacts <- unlist(lapply(igraph::neighborhood(.data, nodes = mark),
                            function(x) setdiff(x, mark)))
  # count exposures for each node:
  tabcontact <- table(contacts)
  out <- rep(0, manynet::network_nodes(.data))
  out[as.numeric(names(tabcontact))] <- unname(tabcontact)
  make_node_measure(out, .data)
}

#' @rdname node_diffusion 
#' @export
node_is_exposed <- function(.data, mark){
  if(is.logical(mark)) mark <- which(mark)
  out <- rep(F, manynet::network_nodes(.data))
  out[unique(setdiff(unlist(igraph::neighborhood(.data, nodes = mark)),
                     mark))] <- TRUE
  make_node_mark(out, .data)
}
