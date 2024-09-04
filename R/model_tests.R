# Tests of network measures ####

#' Tests of network measures
#' 
#' @description
#'   These functions conduct tests of any network-level statistic:
#'   
#'   - `test_random()` performs a conditional uniform graph (CUG) test
#'   of a measure against a distribution of measures on random networks 
#'   of the same dimensions.
#'   - `test_permutation()` performs a quadratic assignment procedure (QAP) test 
#'   of a measure against a distribution of measures on permutations 
#'   of the original network.
#'   
#' @name tests
#' @inheritParams regression
#' @family models
#' @param FUN A graph-level statistic function to test.
#' @param ... Additional arguments to be passed on to FUN,
#'   e.g. the name of the attribute.
NULL

#' @rdname tests 
#' @importFrom manynet generate_random bind_node_attributes is_directed is_complex
#' @examples 
#' marvel_friends <- to_unsigned(ison_marvel_relationships)
#' marvel_friends <- to_giant(marvel_friends) %>% 
#'   to_subgraph(PowerOrigin == "Human")
#' # (cugtest <- test_random(marvel_friends, manynet::net_heterophily, attribute = "Attractive",
#' #   times = 200))
#' # plot(cugtest)
#' @export
test_random <- function(.data, FUN, ..., 
                        times = 1000, 
                        strategy = "sequential", 
                        verbose = FALSE){
  args <- unlist(list(...))
  if (!is.null(args)) {
    obsd <- FUN(.data, args)
  } else {
    obsd <- FUN(.data)
  }
  oplan <- future::plan(strategy)
  on.exit(future::plan(oplan), add = TRUE)
  rands <- furrr::future_map(1:times, manynet::generate_random, n = .data, 
                             .progress = verbose, 
                             .options = furrr::furrr_options(seed = T))
  if (length(args) > 0) {
    rands <- furrr::future_map(rands, 
                               manynet::bind_node_attributes, object2 = .data, 
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
              mode = manynet::is_directed(.data),
              diag = manynet::is_complex(.data),
              cmode = "edges",
              plteobs = mean(simd <= obsd),
              pgteobs = mean(simd >= obsd),
              reps = times)
  class(out) <- "network_test"
  out
}

#' @rdname tests 
#' @examples 
#' # (qaptest <- test_permutation(marvel_friends, 
#' #                 manynet::net_heterophily, attribute = "Attractive",
#' #                 times = 200))
#' # plot(qaptest)
#' @export
test_permutation <- function(.data, FUN, ..., 
                             times = 1000, 
                             strategy = "sequential", 
                             verbose = FALSE){
  args <- unlist(list(...))
  if (!is.null(args)) {
    obsd <- FUN(.data, args)
  } else {
    obsd <- FUN(.data)
  }
  n <- manynet::net_dims(.data)
  d <- manynet::net_density(.data)
  oplan <- future::plan(strategy)
  on.exit(future::plan(oplan), add = TRUE)
  rands <- furrr::future_map(1:times, 
                  function(x) manynet::to_permuted(.data), 
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
              mode = manynet::is_directed(.data),
              diag = manynet::is_complex(.data),
              plteobs = mean(simd <= obsd),
              pgteobs = mean(simd >= obsd),
              reps = times)
  class(out) <- "network_test"
  out
}

#' @export
print.network_test <- function(x, ...,
                             max.length = 6,
                             digits = 3){
  cat(paste("\n", x$test, "Test Results\n\n"))
  cat("Observed Value:", x$testval, "\n")
  cat("Pr(X>=Obs):", x$pgteobs, "\n")
  cat("Pr(X<=Obs):", x$plteobs, "\n\n")
}

#' @export
plot.network_test <- function(x, ...,
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
                        color="red", linewidth=1.2) + ggplot2::ylab("Density")
}

# Tests of network distributions ####

#' Tests of network distributions
#' 
#' @description
#'   These functions conduct tests of distributions:
#'   
#'   - `test_distribution()` performs a two-sample Kolmogorov-Smirnov test on 
#'   whether two "diff_model" objects are drawn from the same distribution.
#'   - `test_fit()` performs a chi-squared test on the squared Mahalanobis distance 
#'   between a diff_model and diff_models objects.
#'   
#' @name test_distributions
#' @family models
NULL

#' @rdname test_distributions 
#' @param diff_model1,diff_model2 diff_model objects
#' @examples
#'  # test_distribution(play_diffusion(ison_networkers), 
#'  #                   play_diffusion(ison_networkers, thresholds = 75))
#' @export
test_distribution <- function(diff_model1, diff_model2){
  out <- stats::ks.test(diff_model1$I, diff_model2$I)
  dplyr::tibble(statistic = out$statistic, p.value = out$p.value, 
                nobs = length(diff_model1$I))
}

#' @rdname test_distributions 
#' @param diff_model A diff_model object is returned by
#'   `play_diffusion()` or `as_diffusion()` and contains
#'   a single empirical or simulated diffusion.
#' @param diff_models A diff_models object is returned by
#'   `play_diffusions()` and contains a series of diffusion simulations.
#' @section Mahalanobis distance: 
#'   `test_gof()` takes a single diff_model object,
#'   which may be a single empirical or simulated diffusion,
#'   and a diff_models object containing many simulations.
#'   Note that currently only the goodness of fit of the 
#'   
#'   It returns a tibble (compatible with `broom::glance()`) that includes
#'   the Mahalanobis distance statistic 
#'   between the observed and simulated distributions.
#'   It also includes a p-value summarising a chi-squared test on this statistic,
#'   listing also the degrees of freedom and number of observations.
#'   If the p-value is less than the convention 0.05,
#'   then one can argue that the first diffusion is not well captured by
#    the set of simulated diffusions (and thus that the model is not a good fit).
#' @examples
#'   # Playing a reasonably quick diffusion
#'   # x <- play_diffusion(generate_random(15), transmissibility = 0.7)
#'   # Playing a slower diffusion
#'   # y <- play_diffusions(generate_random(15), transmissibility = 0.1, times = 40)
#'   # plot(x)
#'   # plot(y)
#'   # test_fit(x, y)
#' @export
test_fit <- function(diff_model, diff_models){ # make into method?
  x <- diff_model
  y <- diff_models
  sim <- `0` <- NULL
  sims <- y |> dplyr::select(sim, t, I)
  sims <- as.data.frame.matrix(stats::xtabs(I ~ sim + t, sims)) # tidyr::pivot_wider replacement
  sims <- sims[,colSums(stats::cov(sims))!=0]
  mah <- stats::mahalanobis(x$I[-1], colMeans(sims), stats::cov(sims))
  pval <- pchisq(mah, df=length(x$I[-1]), lower.tail=FALSE)
  dplyr::tibble(statistic = mah, p.value = pval, 
                df = length(x$I[-1]), nobs = nrow(sims))
}


