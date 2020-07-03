#' European Values Survey, 1990 and 2008
#'
#' 6 two-mode matrices containing individuals' memberships to 14 different types of associations
#' in three countries (Italy, Germany, and the UK) in 1990 and 2008.
#' The Italy data has 658 respondents in 1990 and 540 in 2008.
#' The Germany data has 1369 respondents in 1990 and 503 in 2008.
#' The UK data has 738 respondents in 1990 and 664 in 2008.
#'
#' @docType data
#' @keywords datasets
#' @name evs
#' @usage data(IT_1990)
#' @format Data frames with 15 variables:
#' \describe{
#'   \item{ID}{Anonymised individual identifiers}
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
#' @source Knoke, Christopoulos, Diani, and Hollway. 2020. Multimodal Political Networks. Cambridge University Press: Cambridge.
"IT_1990"

#' @rdname evs
#' @usage data(IT_2008)
"IT_2008"

#' @rdname evs
#' @usage data(DE_1990)
"DE_1990"

#' @rdname evs
#' @usage data(DE_2008)
"DE_2008"

#' @rdname evs
#' @usage data(UK_1990)
"UK_1990"

#' @rdname evs
#' @usage data(UK_2008)
"UK_2008"