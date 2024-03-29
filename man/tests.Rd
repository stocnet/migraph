% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_tests.R
\name{tests}
\alias{tests}
\alias{test_random}
\alias{test_permutation}
\alias{test_gof}
\title{Conditional uniform graph and permutation tests}
\usage{
test_random(
  .data,
  FUN,
  ...,
  times = 1000,
  strategy = "sequential",
  verbose = FALSE
)

test_permutation(
  .data,
  FUN,
  ...,
  times = 1000,
  strategy = "sequential",
  verbose = FALSE
)

test_gof(diff_model, diff_models)
}
\arguments{
\item{.data}{An object of a \code{{manynet}}-consistent class:
\itemize{
\item matrix (adjacency or incidence) from \code{{base}} R
\item edgelist, a data frame from \code{{base}} R or tibble from \code{{tibble}}
\item igraph, from the \code{{igraph}} package
\item network, from the \code{{network}} package
\item tbl_graph, from the \code{{tidygraph}} package
}}

\item{FUN}{A graph-level statistic function to test.}

\item{...}{Additional arguments to be passed on to FUN,
e.g. the name of the attribute.}

\item{times}{Integer indicating number of simulations used for quantile estimation.
(Relevant to the null hypothesis test only -
the analysis itself is unaffected by this parameter.)
Note that, as for all Monte Carlo procedures, convergence is slower for more
extreme quantiles.
By default, \code{times=1000}.
1,000 - 10,000 repetitions recommended for publication-ready results.}

\item{strategy}{If \code{{furrr}} is installed,
then multiple cores can be used to accelerate the function.
By default \code{"sequential"},
but if multiple cores available,
then \code{"multisession"} or \code{"multicore"} may be useful.
Generally this is useful only when \code{times} > 1000.
See \href{https://furrr.futureverse.org}{\code{{furrr}}} for more.}

\item{verbose}{Whether the function should report on its progress.
By default FALSE.
See \href{https://progressr.futureverse.org}{\code{{progressr}}} for more.}

\item{diff_model}{A diff_model object is returned by
\code{play_diffusion()} or \code{as_diffusion()} and contains
a single empirical or simulated diffusion.}

\item{diff_models}{A diff_models object is returned by
\code{play_diffusions()} and contains a series of diffusion simulations.}
}
\description{
These functions conduct tests of any network-level statistic:
\itemize{
\item \code{test_random()} performs a conditional uniform graph (CUG) test
of a measure against a distribution of measures on random networks
of the same dimensions.
\item \code{test_permutation()} performs a quadratic assignment procedure (QAP) test
of a measure against a distribution of measures on permutations
of the original network.
\item \code{test_gof()} performs a chi-squared test on the squared Mahalanobis distance
between a diff_model and diff_models objects.
}
}
\section{Mahalanobis distance}{

\code{test_gof()} takes a single diff_model object,
which may be a single empirical or simulated diffusion,
and a diff_models object containing many simulations.
Note that currently only the goodness of fit of the

It returns a tibble (compatible with \code{broom::glance()}) that includes
the Mahalanobis distance statistic
between the observed and simulated distributions.
It also includes a p-value summarising a chi-squared test on this statistic,
listing also the degrees of freedom and number of observations.
If the p-value is less than the convention 0.05,
then one can argue that the first diffusion is not well captured by
}

\examples{
marvel_friends <- to_unsigned(ison_marvel_relationships)
marvel_friends <- to_giant(marvel_friends) \%>\% 
  to_subgraph(PowerOrigin == "Human")
(cugtest <- test_random(marvel_friends, network_heterophily, attribute = "Attractive",
  times = 200))
plot(cugtest)
(qaptest <- test_permutation(marvel_friends, 
                network_heterophily, attribute = "Attractive",
                times = 200))
plot(qaptest)
  # Playing a reasonably quick diffusion
  x <- play_diffusion(generate_random(15), transmissibility = 0.7)
  # Playing a slower diffusion
  y <- play_diffusions(generate_random(15), transmissibility = 0.1, times = 40)
  plot(x)
  plot(y)
  test_gof(x, y)
}
\seealso{
Other models: 
\code{\link{regression}}
}
\concept{models}
