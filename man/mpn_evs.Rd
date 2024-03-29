% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_mpn.R
\docType{data}
\name{mpn_evs}
\alias{mpn_evs}
\alias{mpn_IT_1990}
\alias{mpn_IT_2008}
\alias{mpn_DE_1990}
\alias{mpn_DE_2008}
\alias{mpn_UK_1990}
\alias{mpn_UK_2008}
\title{Two-mode European Values Survey, 1990 and 2008 (EVS 2020)}
\format{
\code{tbl_graph} object based on an association matrix with 14 columns:
\describe{
\item{Welfare}{1 if individual associated}
\item{Religious}{1 if individual associated}
\item{Education.culture}{1 if individual associated}
\item{Unions}{1 if individual associated}
\item{Parties}{1 if individual associated}
\item{Local.political.groups}{1 if individual associated}
\item{Human.rights}{1 if individual associated}
\item{Environmental.animal}{1 if individual associated}
\item{Professional}{1 if individual associated}
\item{Youth}{1 if individual associated}
\item{Sports}{1 if individual associated}
\item{Women}{1 if individual associated}
\item{Peace}{1 if individual associated}
\item{Health}{1 if individual associated}
}

An object of class \code{tbl_graph} (inherits from \code{igraph}) of length 672.

An object of class \code{tbl_graph} (inherits from \code{igraph}) of length 554.

An object of class \code{tbl_graph} (inherits from \code{igraph}) of length 1383.

An object of class \code{tbl_graph} (inherits from \code{igraph}) of length 517.

An object of class \code{tbl_graph} (inherits from \code{igraph}) of length 752.

An object of class \code{tbl_graph} (inherits from \code{igraph}) of length 678.
}
\source{
Knoke, David, Mario Diani, James Hollway, and Dimitris C Christopoulos. 2021.
\href{https://www.cambridge.org/core/books/multimodal-political-networks/43EE8C192A1B0DCD65B4D9B9A7842128}{\emph{Multimodal Political Networks}}.
Cambridge University Press. Cambridge University Press.
}
\usage{
data(mpn_IT_1990)

data(mpn_IT_1990)

data(mpn_IT_2008)

data(mpn_DE_1990)

data(mpn_DE_2008)

data(mpn_UK_1990)

data(mpn_UK_2008)
}
\description{
6 two-mode matrices containing individuals' memberships to 14 different types
of associations in three countries (Italy, Germany, and the UK) in 1990
and 2008. The Italy data has 658 respondents in 1990 and 540 in 2008.
The Germany data has 1369 respondents in 1990 and 503 in 2008.
The UK data has 738 respondents in 1990 and 664 in 2008.
}
\references{
EVS (2020). European Values Study Longitudinal Data File 1981-2008 (EVS 1981-2008).
GESIS Data Archive, Cologne. ZA4804 Data file Version 3.1.0,
\doi{10.4232/1.13486}.
}
\keyword{datasets}
