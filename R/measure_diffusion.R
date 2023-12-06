#' Functions to measure diffusion processes on networks
#' @param diff_model A valid network diffusion model.
#' @family measures
#' @name diffusion
#' @examples
#'   smeg <- manynet::generate_smallworld(15, 0.025)
#'   smeg_diff <- play_diffusion(smeg, recovery = 0.2)
#'   plot(smeg_diff)
#'   (adopts <- node_adopter(smeg_diff))
#'   summary(adopts)
#'   summary(node_adoption_time(smeg_diff), membership = adopts)
#'   summary(node_thresholds(smeg_diff), membership = adopts)
#'   summary(node_infection_length(smeg_diff))
#'   network_infection_length(smeg_diff)
#' @references
#'   Kermack, W. and McKendrick, A., 1927. "A contribution to the mathematical theory of epidemics". 
#'   _Proc. R. Soc. London A_ 115: 700-721.
#'   
#'   Valente, Tom W. (1995). _Network models of the diffusion of innovations_
#'   (2nd ed.). Cresskill N.J.: Hampton Press.
NULL

#' @describeIn diffusion Calculates the average transmissibility observed
#'   in a diffusion simulation, or the number of new infections over
#'   the number of susceptible, over the number of infected 
#' @export
network_transmissibility <- function(diff_model){
  out <- diff_model |> 
    mutate(transmissibility = (I - dplyr::lag(I)/dplyr::lag(S))/
             dplyr::lag(I))
  out <- out$transmissibility
  out <- out[!is.infinite(out)]
  mean(out, na.rm = TRUE)
}

#' @describeIn diffusion Measures the average length nodes remain
#'   infected in a compartmental model with recovery for the network as a whole
#' @export
network_infection_length <- function(diff_model){
  make_network_measure(mean(node_infection_length(diff_model), na.rm = TRUE),
                       attr(diff_model, "network"))
}

#' @describeIn diffusion Calculates the observed reproductive number
#'   in a diffusion simulation as the network's transmissibility over
#'   the network's average infection length
#' @export
network_reproduction <- function(diff_model){
  network_transmissibility(diff_model)/
    network_infection_length(diff_model)
}

#' @describeIn diffusion Measures nodes' time of adoption/infection
#' @export
node_adoption_time <- function(diff_model){
  out <- summary(diff_model) |> dplyr::filter(event == "I") |> 
    dplyr::distinct(nodes, .keep_all = TRUE) |> 
    dplyr::select(t) |> c() |> unname() |> unlist()
  make_node_measure(out, attr(diff_model, "network"))
}

#' @describeIn diffusion Classifies membership of nodes into diffusion categories
#' @export
node_adopter <- function(diff_model){
  toa <- node_adoption_time(diff_model)
  avg <- mean(toa)
  sdv <- sd(toa)
  out <- ifelse(toa < (avg - sdv), "Early Adopter", 
         ifelse(toa > (avg + sdv), "Laggard",
                ifelse((avg - sdv) < toa & toa <= avg, "Early Majority", 
                       ifelse(avg < toa & toa <= avg + sdv, "Late Majority", "Non-Adopter"))))
  make_node_member(out, attr(diff_model, "network"))
}

#' @describeIn diffusion Measures nodes' thresholds from the amount
#'   of exposure they had when they became infected
#' @export
node_thresholds <- function(diff_model){
  exposure <- NULL
  out <- summary(diff_model)
  if(any(out$event == "E")) 
    out <- out |> dplyr::filter(event == "E") else 
      out <- out |> dplyr::filter(event == "I")
  out <- out |> dplyr::distinct(nodes, .keep_all = TRUE) |> 
    dplyr::select(exposure) |> c() |> unname() |> unlist()
  make_node_measure(out, attr(diff_model, "network"))
}

#' @describeIn diffusion Measures the average length nodes that become
#'   infected remain infected in a compartmental model with recovery
#' @export
node_infection_length <- function(diff_model){
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

