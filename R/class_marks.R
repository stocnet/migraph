make_tie_mark <- function(out, object) {
  class(out) <- c("tie_mark", class(out))
  attr(out, "mode") <- node_mode(object)
  out
}

#' @export
print.tie_mark <- function(x, ...,
                              max.length = 6,
                              digits = 3) {
  z <- x[1:min(length(x), max.length)]
  class(z) <- "logical"
  z <- format(z, digits = digits)
  print(noquote(format(c(z,
                         paste("+", length(x) - length(z), 
                               "others")))))
}

