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
  y <- as.logical(x)
  print_tblvec(y, names)
}

# make tblvec ####
print_tblvec <- function(y, names){
  mat <- matrix(y, dimnames = names)
  mat <- t(mat)
  out <- as.data.frame(mat)
  tibs <- dplyr::tibble(out, .name_repair = "unique")
  setup <- pillar::tbl_format_setup(tibs)
  body <- pillar::tbl_format_body(tibs, setup)[c(1,3)]
  if(setup$extra_cols_total > 0){
    print(body)
    cat(pillar::style_subtle(paste("# ... with",
                                   setup$extra_cols_total,
                                   "more in the vector.")))
  } else print(body)
}
