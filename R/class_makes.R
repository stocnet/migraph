make_network_measures <- function(out, .data) {
  out <- dplyr::as_tibble(out) |> 
    dplyr::mutate(time = as.numeric(names(out))) |> 
    dplyr::select(time, value)
  class(out) <- c("network_measures", class(out))
  attr(out, "mode") <- manynet::net_dims(.data)
  out
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
  invisible(x)
}


