make_measure <- function(out, object){
  class(out) <- c("measure", class(out))
  attr(out, "mode") <- node_mode(object)
  out
}

#' @export
print.measure <- function(x, ..., 
                          max.length = 6,
                          digits = 3){
  if(any(attr(x, "mode"))){
    y <- x[attr(x, "mode")]
    y <- y[max(1, ((length(y)-max.length)+1)):length(y)]
    x <- x[!attr(x, "mode")]
    x <- x[1:min(length(x), max.length)]
    class(x) <- "numeric"
    x <- format(x, digits = digits)
    class(y) <- "numeric"
    y <- format(y, digits = digits)
    print(noquote(format(c(x, "...", y))))
  } else {
    x <- x[1:min(length(x), max.length)]
    class(x) <- "numeric"
    x <- format(x, digits = digits)
    print(noquote(format(c(x, "..."))))
  }
}
