#' Conditional uniform graph and permutation tests
#' 
#' These functions conduct conditional uniform graph (CUG) 
#' or permutation (QAP) tests of any graph-level statistic.
#' @name tests
#' @inheritParams as_igraph
#' @param FUN A graph-level statistic function to test.
#' @param ... Additional arguments to be passed on to FUN,
#'   e.g. the name of the attribute.
#' @param times Integer indicating the number of draws to use for quantile
#'   estimation.
#'   By default, times=1000.
#'   1,000 - 10,000 repetitions recommended for publication-ready results.
#' @param parallel If `{furrr}` is installed, 
#'   then multiple cores can be used to accelerate the function.
#'   By default FALSE.
#'   Recommended if `times` > 1000.
NULL

#' @rdname tests
#' @examples 
#' marvel_friends <- to_unsigned(ison_marvel_relationships)
#' marvel_friends <- to_main_component(marvel_friends) %>% 
#'   filter(PowerOrigin == "Human")
#' (cugtest <- test_random(marvel_friends, graph_ei_index, attribute = "Attractive",
#'   times = 2000))
#' plot(cugtest)
#' @export
test_random <- function(object, FUN, ..., 
                        times = 1000, 
                        strategy = "sequential", 
                        verbose = FALSE){
  args <- unlist(list(...))
  if (!is.null(args)) {
    obsd <- FUN(object, args)
  } else {
    obsd <- FUN(object)
  }
  n <- graph_nodes(object)
  d <- graph_density(object)
  future::plan(strategy)
  rands <- furrr::future_map(1:times, generate_random, n = n, p = d, 
                             .progress = verbose, 
                             .options = furrr::furrr_options(seed = T))
  if (length(args) > 0) {
    rands <- furrr::future_map(rands, 
                               copy_node_attributes, object2 = object, 
                               .progress = verbose, 
                               .options = furrr::furrr_options(seed = T))
  }
  if (!is.null(args)) {
    simd <- furrr::future_map_dbl(rands,
                  FUN, args)
  } else {
    simd <- furrr::future_map_dbl(rands,
                   FUN)
  }
  out <- list(obs.stat = obsd,
              rep.stat = simd,
              mode = is_directed(object),
              diag = is_complex(object),
              cmode = "csize",
              plteobs = mean(simd <= obsd),
              pgteobs = mean(simd >= obsd),
              reps = times)
  class(out) <- "cug.test"
  out
}
#' @rdname tests
#' @importFrom sna rmperm
#' @examples 
#' (qaptest <- test_permutation(marvel_friends, graph_ei_index, attribute = "Attractive",
#'   times = 200))
#' plot(qaptest)
#' @export
test_permutation <- function(object, FUN, ..., 
                             times = 1000, 
                             strategy = "sequential", 
                             verbose = FALSE){
  args <- unlist(list(...))
  if (!is.null(args)) {
    obsd <- FUN(object, args)
  } else {
    obsd <- FUN(object)
  }
  n <- graph_nodes(object)
  d <- graph_density(object)
  future::plan(strategy)
  rands <- furrr::future_map(1:times, 
                  function(x) as_igraph(generate_permutation(object)), 
                  .progress = verbose, 
                  .options = furrr::furrr_options(seed = T))
  if (!is.null(args)) {
    simd <- furrr::future_map_dbl(rands,
                   FUN, args, 
                   .progress = verbose, 
                   .options = furrr::furrr_options(seed = T))
  } else {
    simd <- furrr::future_map_dbl(rands,
                   FUN, 
                   .progress = verbose, 
                   .options = furrr::furrr_options(seed = T))
  }
  out <- list(testval = obsd,
              dist = simd,
              plteobs = mean(simd <= obsd),
              pgteobs = mean(simd >= obsd),
              reps = times)
  class(out) <- "qap.test"
  out
}
