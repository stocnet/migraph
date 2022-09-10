#' Conditional uniform graph and permutation tests
#' 
#' These functions conduct conditional uniform graph (CUG) 
#' or permutation (QAP) tests of any graph-level statistic.
#' @name tests
#' @inheritParams regression
#' @family models
#' @param FUN A graph-level statistic function to test.
#' @param ... Additional arguments to be passed on to FUN,
#'   e.g. the name of the attribute.
NULL

#' @describeIn tests Returns test results for some measure on an object
#'   against a distribution of measures on random networks of the same dimensions
#' @examples 
#' marvel_friends <- to_unsigned(ison_marvel_relationships)
#' marvel_friends <- to_giant(marvel_friends) %>% 
#'   to_subgraph(PowerOrigin == "Human")
#' (cugtest <- test_random(marvel_friends, graph_homophily, attribute = "Attractive",
#'   times = 200))
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
  out <- list(test = "CUG",
              testval = obsd,
              testdist = simd,
              mode = is_directed(object),
              diag = is_complex(object),
              cmode = "csize",
              plteobs = mean(simd <= obsd),
              pgteobs = mean(simd >= obsd),
              reps = times)
  class(out) <- "graph_test"
  out
}
#' @describeIn tests Returns test results for some measure on an object
#'   against a distribution of measures on permutations of the original network
#' @examples 
#' (qaptest <- test_permutation(marvel_friends, 
#'                 graph_homophily, attribute = "Attractive",
#'                 times = 200))
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
  out <- list(test = "QAP",
              testval = obsd,
              testdist = simd,
              mode = is_directed(object),
              diag = is_complex(object),
              plteobs = mean(simd <= obsd),
              pgteobs = mean(simd >= obsd),
              reps = times)
  class(out) <- "graph_test"
  out
}

#' @export
print.graph_test <- function(x, ...,
                             max.length = 6,
                             digits = 3){
  cat(paste("\n", x$test, "Test Results\n\n"))
  cat("Observed Value:", x$testval, "\n")
  cat("Pr(X>=Obs):", x$pgteobs, "\n")
  cat("Pr(X<=Obs):", x$plteobs, "\n\n")
}

#' @export
plot.graph_test <- function(x, ...,
                            threshold = .95, 
                            tails = c("two", "one")){
  data <- data.frame(Statistic = x$testdist)
  p <- ggplot2::ggplot(data, 
                       ggplot2::aes(x = .data$Statistic)) + 
    ggplot2::geom_density()
  if(all(data$Statistic >= -1 & data$Statistic <= 1)){
    p <- p + ggplot2::expand_limits(x=0) + 
      ggplot2::geom_vline(ggplot2::aes(xintercept = 0),
                          linetype="dashed")
    if(any(data$Statistic < 0)) p <- p + ggplot2::expand_limits(x=-1)
    if(any(data$Statistic > 0)) p <- p + ggplot2::expand_limits(x=1)
  }
  d <- ggplot2::ggplot_build(p)$data[[1]]
  tails = match.arg(tails)
  if(tails == "one"){
    if(x$testval < quantile(data$Statistic, .5)){
      thresh <- quantile(data$Statistic, 1 - threshold)
      p <- p + ggplot2::geom_area(data = subset(d, x < thresh), 
                                  aes(x = x, y = .data$y), fill = "lightgrey")
    } else {
      thresh <- quantile(data$Statistic, threshold)
      p <- p + ggplot2::geom_area(data = subset(d, x > thresh), 
                                  aes(x = x, y = .data$y), fill = "lightgrey")
    }
  } else if (tails == "two"){
    thresh <- quantile(data$Statistic, 
                       c((1-threshold)/2, ((1-threshold)/2)+threshold))
    p <- p + ggplot2::geom_area(data = subset(d, x < thresh[1]), 
                                aes(x = x, y = .data$y), fill = "lightgrey") + 
      ggplot2::geom_area(data = subset(d, x > thresh[2]), 
                         aes(x = x, y = .data$y), fill = "lightgrey")
  }
  p + ggplot2::theme_classic() + ggplot2::geom_density() +
    ggplot2::geom_vline(ggplot2::aes(xintercept = x$testval),
                        color="red", size=1.2) + ggplot2::ylab("Density")
}
