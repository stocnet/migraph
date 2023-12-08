#' Functions to measure diffusion processes on networks
#' @param diff_model A valid network diffusion model.
#' @family measures
#' @name diffusion
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
#'   Kermack, W. and McKendrick, A., 1927. "A contribution to the mathematical theory of epidemics". 
#'   _Proc. R. Soc. London A_ 115: 700-721.
#'   
#'   Valente, Tom W. (1995). _Network models of the diffusion of innovations_
#'   (2nd ed.). Cresskill N.J.: Hampton Press.
NULL

#' @describeIn diffusion Measures the average transmissibility observed
#'   in a diffusion simulation, or the number of new infections over
#'   the number of susceptible nodes
#' @export
network_transmissibility <- function(diff_model){
  out <- diff_model$I_new/diff_model$s
  out <- out[-1]
  out <- out[!is.infinite(out)]
  out <- out[!is.nan(out)]
  make_network_measure(mean(out, na.rm = TRUE),
                       attr(diff_model, "network"))
}

#' @describeIn diffusion Measures the average length nodes remain
#'   infected in a compartmental model with recovery for the network as a whole
#' @export
network_infection_length <- function(diff_model){
  make_network_measure(mean(node_infection_length(diff_model), na.rm = TRUE),
                       attr(diff_model, "network"))
}

#' @describeIn diffusion Measures the observed reproductive number
#'   in a diffusion simulation as the network's transmissibility over
#'   the network's average infection length
#' @export
network_reproduction <- function(diff_model){
  net <- attr(diff_model, "network")
  out <- network_transmissibility(diff_model)/
    (1/network_infection_length(diff_model))
  out <- min(out, mean(node_degree(net, normalized = FALSE)))
  make_network_measure(out, net)
}

#' @describeIn diffusion Measures nodes' time of adoption/infection
#' @export
node_adoption_time <- function(diff_model){
  event <- nodes <- NULL
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
  sdv <- stats::sd(toa)
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

#' @describeIn diffusion Measures the average length nodes that become
#'   infected remain infected in a compartmental model with recovery
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

#' @describeIn diffusion Measures how many exposures nodes have
#'   to a given mark
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

#' @describeIn diffusion Marks the nodes that are susceptible,
#'   i.e. are in the immediate neighbourhood of given mark vector
#' @export
node_is_exposed <- function(.data, mark){
  if(is.logical(mark)) mark <- which(mark)
  out <- rep(F, manynet::network_nodes(.data))
  out[unique(setdiff(unlist(igraph::neighborhood(.data, nodes = mark)),
                     mark))] <- TRUE
  make_node_mark(out, .data)
}

#' @describeIn diffusion Coerces a table of diffusion events into
#'   a `diff_model` object similar to the output of `play_diffusion()`
#' @export
as_diffusion <- function(events, .data){
  net <- .data
  event <- NULL
  sumchanges <- events |> dplyr::group_by(t) |> 
    dplyr::reframe(I_new = sum(event == "I"),
                   E_new = sum(event == "E"),
                   R_new = sum(event == "R"))
  report <- tibble::tibble(t = seq_len(max(events$t)),
                           n = manynet::network_nodes(net))
  report <- dplyr::left_join(report, sumchanges, by = dplyr::join_by(t))
  report[is.na(report)] <- 0
  report$I <- cumsum(report$I_new)
  report$S <- report$n - report$I
  report$s <- vapply(report$t, function(time){
    twin <- dplyr::filter(events, events$t <= time)
    infected <- dplyr::filter(twin, twin$event == "I")$nodes
    recovered <- dplyr::filter(twin, twin$event == "R")$nodes
    infected <- setdiff(infected, recovered)
    expos <- node_is_exposed(net, infected)
    expos[recovered] <- F
    sum(expos)
  }, numeric(1) )
  if(any(report$R_new > 0)){
    report$R <- cumsum(report$R_new)
    report$I <- report$I - report$R
  } else report$R_new <- NULL
  # if(any(report$E_new > 0)){
  report$E <- cumsum(report$E_new) - report$I
  report$E[report$E < 0] <- 0
  report$S <- report$n - (report$E + report$I)
  # } else report$E_new <- NULL
  report <- dplyr::relocate(report, dplyr::any_of(c("t", "n", "S", "E", "I", "R")))
  make_diff_model(events, report, .data)
}
