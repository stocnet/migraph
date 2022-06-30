make_tie_mark <- function(out, object) {
  class(out) <- c("tie_mark", class(out))
  # attr(out, "mode") <- node_mode(object)
  out
}

#' @export
print.tie_mark <- function(x, ...,
                              max.length = 6,
                              digits = 3) {
  names <- list(names(x))
  x <- as.logical(x)
  mat <- matrix(x, dimnames = names)
  mat <- t(mat)
  out <- as.data.frame(mat)
  print(dplyr::tibble(out, .name_repair = "unique"))
}

