#' Two-mode European Values Survey, 1990 and 2008
#'
#' 6 two-mode matrices containing individuals' memberships to 14 different types
#' of associations in three countries (Italy, Germany, and the UK) in 1990
#' and 2008. The Italy data has 658 respondents in 1990 and 540 in 2008.
#' The Germany data has 1369 respondents in 1990 and 503 in 2008.
#' The UK data has 738 respondents in 1990 and 664 in 2008.
#'
#' @docType data
#' @keywords datasets
#' @name mpn_evs
#' @usage data(mpn_IT_1990)
#' @format Matrices with 14 columns:
#' \describe{
#'   \item{Welfare}{1 if individual associated}
#'   \item{Religious}{1 if individual associated}
#'   \item{Education.culture}{1 if individual associated}
#'   \item{Unions}{1 if individual associated}
#'   \item{Parties}{1 if individual associated}
#'   \item{Local.political.groups}{1 if individual associated}
#'   \item{Human.rights}{1 if individual associated}
#'   \item{Environmental.animal}{1 if individual associated}
#'   \item{Professional}{1 if individual associated}
#'   \item{Youth}{1 if individual associated}
#'   \item{Sports}{1 if individual associated}
#'   \item{Women}{1 if individual associated}
#'   \item{Peace}{1 if individual associated}
#'   \item{Health}{1 if individual associated}
#' }
#' @source Knoke, Diani, Hollway, and Christopoulos. 2021. \emph{Multimodal Political Networks}. Cambridge University Press: Cambridge.
"mpn_IT_1990"

#' @rdname mpn_evs
#' @usage data(mpn_IT_2008)
"mpn_IT_2008"

#' @rdname mpn_evs
#' @usage data(mpn_DE_1990)
"mpn_DE_1990"

#' @rdname mpn_evs
#' @usage data(mpn_DE_2008)
"mpn_DE_2008"

#' @rdname mpn_evs
#' @usage data(mpn_UK_1990)
"mpn_UK_1990"

#' @rdname mpn_evs
#' @usage data(mpn_UK_2008)
"mpn_UK_2008"