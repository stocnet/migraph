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
#'  test_distribution(as_diffusion(play_diffusion(ison_networkers)),
#'              as_diffusion(play_diffusion(ison_networkers, thresholds = 75)))
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
  sims <- y %>% dplyr::select(sim, t, I)
  sims <- as.data.frame.matrix(stats::xtabs(I ~ sim + t, sims)) # tidyr::pivot_wider replacement
  sims <- sims[,colSums(stats::cov(sims))!=0]
  mah <- stats::mahalanobis(x$I[-1], colMeans(sims), stats::cov(sims))
  pval <- pchisq(mah, df=length(x$I[-1]), lower.tail=FALSE)
  dplyr::tibble(statistic = mah, p.value = pval, 
                df = length(x$I[-1]), nobs = nrow(sims))
}
