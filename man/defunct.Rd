% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/migraph-defunct.R
\name{defunct}
\alias{defunct}
\alias{test_gof}
\alias{network_reg}
\title{Functions that have been renamed, superseded, or are no longer working}
\usage{
test_gof(diff_model, diff_models)

network_reg(
  formula,
  .data,
  method = c("qap", "qapy"),
  times = 1000,
  strategy = "sequential",
  verbose = FALSE
)
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}
Generally these functions have been superseded or renamed.
Upon using them, a message is provided directing the user to the new function.
However, at this stage of package development,
we generally clear older defunct functions at each minor release,
and so you are strongly encouraged to use the new functions/names/syntax
wherever possible and update your scripts accordingly.
}
\section{Functions}{
\itemize{
\item \code{test_gof()}: Deprecated on 2024-06-16.

\item \code{network_reg()}: Deprecated on 2024-07-19.

}}
\keyword{internal}
