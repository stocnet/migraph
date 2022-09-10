make_node_mark <- function(out, object) {
  class(out) <- c("node_mark", class(out))
  if (is.null(names(out)) && is_labelled(object))
    names(out) <- node_names(object)
  attr(out, "mode") <- node_mode(object)
  out
}

make_tie_mark <- function(out, object) {
  class(out) <- c("tie_mark", class(out))
  out
}

#' @export
print.node_mark <- function(x, ...,
                            max.length = 6,
                            digits = 3) {
  if (any(attr(x, "mode"))) {
    for(m in c(FALSE, TRUE)){
      print_tblvec(y = as.logical(x)[attr(x, "mode") == m],
                   names = list(names(x)[attr(x, "mode") == m]))
      if (!m) cat("\n")
    }
  } else {
    print_tblvec(y = as.logical(x),
                 names = list(names(x)))
  }
}

#' @export
print.tie_mark <- function(x, ...,
                           max.length = 6,
                           digits = 3) {
  print_tblvec(y = as.logical(x),
               names = list(names(x)))
}

