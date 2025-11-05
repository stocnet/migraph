make_network_measures <- function(out, .data) {
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

#' @export
summary.diffs_model <- function(object, ...) {
  object %>% dplyr::mutate(fin = (I!=n)*1) %>% 
    dplyr::group_by(sim) %>% dplyr::summarise(toa = sum(fin)+1)
}

#' @export
print.diffs_model <- function(x, ...){
  x <- x[,colSums(x, na.rm=TRUE) != 0]
  x$I_new <- NULL
  print(dplyr::tibble(x, ...))
}

## Over ####

make_over_memb <- function(out, categ) {
  names(out) <- categ
  class(out) <- c("over_memb", class(out))
  out
}

#' @export
print.over_memb <- function(x, ...){
  print(round(unclass(x),3), ...)
}


