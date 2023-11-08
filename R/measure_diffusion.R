#' Functions to play games on networks
#' @param diff_model A valid network diffusion model.
#' @family measures
#' @name diffusion
#' @references
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

#' @describeIn diffusion Calculates the average length nodes remain
#'   infected in a compartmental model with recovery
#' @export
node_infection_length <- function(diff_model){
  events <- attr(diff_model, "events")
  if(!"R" %in% events$event) stop("Infection length only calculable if there is recovery or removal.")
  vapply(seq_len(diff_model$n[1]), 
         function(x) mean(diff(dplyr::filter(events, nodes == x)$t)),
         FUN.VALUE = numeric(1))
}

#' @describeIn diffusion Calculates the average length nodes remain
#'   infected in a compartmental model with recovery for the network as a whole
#' @export
network_infection_length <- function(diff_model){
  mean(node_infection_length(diff_model))
}

#' @describeIn diffusion Calculates the observed reproductive number
#'   in a diffusion simulation as the network's transmissibility over
#'   the network's average infection length
#' @export
network_reproduction <- function(diff_model){
  network_transmissibility(diff_model)/
    network_infection_length(diff_model)
}

#' @describeIn diffusion Returns nodes' time of adoption/infection
#' @export
node_adoption_time <- function(diff_model){
  summary(diff_model) |> dplyr::filter(event == "I") |> 
    dplyr::distinct(nodes, .keep_all = TRUE) |> 
    dplyr::select(t) |> c() |> unname() |> unlist()
}

#' @describeIn diffusion Returns nodes' time of adoption/infection
#' @export
node_adopter <- function(diff_model){
  toa <- node_adoption_time(diff_model)
  avg <- mean(toa)
  sdv <- sd(toa)
  ifelse(toa < (avg - sdv), "Early Adopter", 
         ifelse(toa > (avg + sdv), "Laggard",
                ifelse((avg - sdv) < toa & toa <= avg, "Early Majority", 
                       ifelse(avg < toa & toa <= avg + sdv, "Late Majority", "Non-Adopter"))))
}

#' @describeIn diffusion Infers nodes' thresholds from the amount
#'   of exposure they had when they became infected
#' @export
node_thresholds <- function(diff_model){
  summary(diff_model) |> dplyr::filter(event == "I") |> 
    dplyr::distinct(nodes, .keep_all = TRUE) |> 
    dplyr::select(exposure) |> c() |> unname() |> unlist()
}