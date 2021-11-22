#' Conditional uniform graph and permutation tests
#' 
#' These functions conduct conditional uniform graph (CUG) 
#' or permutation (QAP) tests of any graph-level statistic.
#' @name tests
#' @inheritParams as_igraph
#' @param FUN A graph-level statistic function to test.
#' @param ... Additional arguments to be passed on to FUN,
#'   e.g. the name of the attribute.
#' @param nSim The number of Monte Carlo simulations to perform.
NULL

#' @rdname tests
#' @examples 
#' marvel_friends <- to_unsigned(ison_marvel_relationships)
#' marvel_friends <- to_main_component(marvel_friends) %>% 
#'   filter(PowerOrigin == "Human")
#' (cugtest <- test_random(marvel_friends, graph_ei_index, attribute = "Attractive",
#'   nSim = 200))
#' plot(cugtest)
#' @export
test_random <- function(object, FUN, ..., nSim = 1000){
  args <- unlist(list(...))
  if (!is.null(args)) {
    obsd <- FUN(object, args)
  } else {
    obsd <- FUN(object)
  }
  n <- graph_nodes(object)
  d <- graph_density(object)
  rands <- lapply(1:nSim, generate_random, n = n, p = d)
  if (length(args) > 0) {
    rands <- lapply(rands, copy_node_attributes, object2 = object)
  }
  if (!is.null(args)) {
    simd <- vapply(rands,
                  FUN, args, FUN.VALUE = numeric(1))
  } else {
    simd <- vapply(rands,
                   FUN, FUN.VALUE = numeric(1))
  }
  out <- list(obs.stat = obsd,
              rep.stat = simd,
              mode = is_directed(object),
              diag = is_complex(object),
              cmode = "csize",
              plteobs = mean(simd <= obsd),
              pgteobs = mean(simd >= obsd),
              reps = nSim)
  class(out) <- "cug.test"
  out
}
#' @rdname tests
#' @importFrom sna rmperm
#' @examples 
#' (qaptest <- test_permutation(marvel_friends, graph_ei_index, attribute = "Attractive",
#'   nSim = 200))
#' plot(qaptest)
#' @export
test_permutation <- function(object, FUN, ..., nSim = 1000){
  args <- unlist(list(...))
  if (!is.null(args)) {
    obsd <- FUN(object, args)
  } else {
    obsd <- FUN(object)
  }
  n <- graph_nodes(object)
  d <- graph_density(object)
  rands <- lapply(1:nSim, function(x) as_igraph(generate_permutation(object)))
  if (!is.null(args)) {
    simd <- vapply(rands,
                   FUN, args, FUN.VALUE = numeric(1))
  } else {
    simd <- vapply(rands,
                   FUN, FUN.VALUE = numeric(1))
  }
  out <- list(testval = obsd,
              dist = simd,
              plteobs = mean(simd <= obsd),
              pgteobs = mean(simd >= obsd),
              reps = nSim)
  class(out) <- "qaptest"
  out
}
