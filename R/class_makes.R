make_network_measures <- function(out, .data) {
  time <- value <- NULL
  out <- dplyr::as_tibble(out) %>% 
    dplyr::mutate(time = as.numeric(names(out))) %>% 
    dplyr::select(time, value)
  class(out) <- c("network_measures", class(out))
  attr(out, "mode") <- manynet::net_dims(.data)
  out
}

make_diffs_model <- function(report, .data) {
  class(report) <- c("diffs_model", class(report))
  attr(report, "mode") <- manynet::node_is_mode(.data)
  report
}


