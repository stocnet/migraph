#' One-mode Mexican power elite database
#'
#' A network of 11 core members of the 1990s Mexican power elite (Knoke 2017),
#' three of which were successively elected presidents of Mexico:
#' José López Portillo (1976-82), Miguel de la Madrid (1982-88), and Carlos Salinas de Gortari (1988-94,
#' who was also the son of another core member, Raúl Salinas Lozano).
#' The undirected lines connecting pairs of men represent any formal, informal,
#' or organizational relation between a dyad;
#' for example, “common belonging (school, sports, business, political participation),
#' or a common interest (political power)” (Mendieta et al. 1997: 37).
#'
#' @docType data
#' @keywords datasets
#' @name mpn_elite_mex
#' @usage data(mpn_elite_mex)
#' @format Matrix with 11 rows/columns
#' @source Knoke, David. 1990. \emph{Political Networks}.
#' 
#' Knoke, David, Mario Diani, James Hollway, and Dimitris C Christopoulos. 2021.
#' \href{https://www.cambridge.org/core/books/multimodal-political-networks/43EE8C192A1B0DCD65B4D9B9A7842128}{\emph{Multimodal Political Networks}}.
#' Cambridge University Press. Cambridge University Press.
"mpn_elite_mex"

#' Two-mode American power elite database
#'
#' A 2-mode network of persons serving as directors or trustees of think tanks.
#' Think tanks are “public-policy research analysis and engagement organizations
#' that generate policy-oriented research, analysis, and advice on domestic and international issues,
#' thereby enabling policymakers and the public to make informed decisions about public policy” (McGann 2016: 6).
#' The Power Elite Database (Domhoff 2016) includes information on the directors of 33 prominent think tanks in 2012.
#' Here we include only 14 directors who held three or more seats among 20 think tanks.
#'
#' @docType data
#' @keywords datasets
#' @name mpn_elite_usa_advice
#' @usage data(mpn_elite_usa_advice)
#' @format Matrix with 14 rows and 20 columns
#' @references
#' Domhoff, G William. 2016. \href{https://whorulesamerica.ucsc.edu/power_elite/}{“Who Rules America? Power Elite Database.”}
#' 
#' Knoke, David, Mario Diani, James Hollway, and Dimitris C Christopoulos. 2021. 
#' \href{https://www.cambridge.org/core/books/multimodal-political-networks/43EE8C192A1B0DCD65B4D9B9A7842128}{\emph{Multimodal Political Networks}}.
#' Cambridge University Press. Cambridge University Press.
"mpn_elite_usa_advice"

#' Three-mode American power elite database
#'
#' This data is based on 26 elites who sat on the boards of directors 
#' for at least two of six economic policy making organizations (Domhoff 2016),
#' and also made campaign contributions to one or more of six candidates 
#' running in the primary election contests for the 2008 Presidential nominations 
#' of the Republican Party (Rudy Giuliani, John McCain, Mitt Romney) 
#' or the Democratic Party (Hillary Clinton, Christopher Dodd, Barack Obama).
#'
#' @docType data
#' @keywords datasets
#' @name mpn_elite_usa_money
#' @usage data(mpn_elite_usa_money)
#' @format Matrix with 26 rows and 6+6 columns
#' @references
#' Domhoff, G William. 2016. \href{https://whorulesamerica.ucsc.edu/power_elite/}{“Who Rules America? Power Elite Database.”}
#' 
#' The Center for Responsive Politics. 2019. \href{http://www.opensecrets.org}{“OpenSecrets.”}
#' 
#' Knoke, David, Mario Diani, James Hollway, and Dimitris C Christopoulos. 2021.
#' \href{https://www.cambridge.org/core/books/multimodal-political-networks/43EE8C192A1B0DCD65B4D9B9A7842128}{\emph{Multimodal Political Networks}}.
#' Cambridge University Press. Cambridge University Press.
"mpn_elite_usa_money"


#' Multimodal (3) Bristol protest events, 1990-2002
#'
#' A multimodal (3) matrix containing individuals affiliations to civic organizations
#' in Bristol and their participation in major protest and civic events between 1990-2002,
#' and the involvement of the organizations in these events.
#'
#' @docType data
#' @keywords datasets
#' @name mpn_bristol
#' @usage data(mpn_bristol)
#' @format A matrix with 264 rows and columns. Node IDs are prefaced with a type identifier:
#' \describe{
#'   \item{1_}{150 Individuals, anonymised}
#'   \item{2_}{97 Bristol Civic Organizations}
#'   \item{3_}{17 Major Protest and Civic Events in Bristol, 1990-2002}
#' }
#' @source Knoke, David, Mario Diani, James Hollway, and Dimitris C Christopoulos. 2021.
#' \href{https://www.cambridge.org/core/books/multimodal-political-networks/43EE8C192A1B0DCD65B4D9B9A7842128}{\emph{Multimodal Political Networks}}.
#' Cambridge University Press. Cambridge University Press.
"mpn_bristol"

#' One-mode EU policy influence network, June 2004
#'
#' Network of anonymised actors reacting to the Ryanair/Charleroi decision of
#' the EU Commission in February 2004.
#' The relationships mapped comprise an account of public records of interaction
#' supplemented with the cognitive network of key informants.
#' Examination of relevant communiques, public statements and a number of
#' off-the-record interviews provides confidence
#' that the network mapped closely approximated interactions between 29 January
#' and 12 February 2004. The time point mapped is at the height of influence and
#' interest intermediation played by actors in the AER,
#' a comparatively obscure body representing the interests of a number of
#' European regional bodies at the EU institutions.
#'
#' @docType data
#' @keywords datasets
#' @name mpn_ryanair
#' @usage data(mpn_ryanair)
#' @format Matrix with 20 rows/columns
#' @source Christopoulos, Dimitrios C. 2006.
#' “Relational Attributes of Political Entrepreneurs: a Network Perspective.”
#' \emph{Journal of European Public Policy} 13 (5): 757–78.
#' 
#' Knoke, Diani, Hollway, and Christopoulos. 2021. \emph{Multimodal Political Networks}. Cambridge University Press: Cambridge.
"mpn_ryanair"

#' Two-mode 112th Congress Senate Voting
#'
#' These datasets list the U.S. Senators who served in the 112th Congress, 
#' which met from January 3, 2011 to January 3, 2013. 
#' Although the Senate has 100 seats, 103 persons served during this period due
#' to two resignations and a death. However, the third replacement occurred
#' only two days before the end and cast no votes on the bills investigated
#' here. Hence, the number of Senators analyzed is 102.
#' 
#' CQ Almanac identified 25 key bills on which the Senate voted during the
#' 112th Congress, and which Democratic and Republican Senators voting “yea”
#' and “nay” on each proposal.
#' 
#' Lastly, we obtained data on campaign contributions made by 92 PACs from the
#' Open Secrets Website. We recorded all contributions made during the 2008,
#' 2010, and 2012 election campaigns to the 102 persons who were Senators in
#' the 112th Congress. The vast majority of PAC contributions to a candidate
#' during a campaign was for $10,000 (the legal maximum is $5,000 each for a
#' primary and the general election). We aggregated the contributions across
#' all three electoral cycles, then dichotomized the sums into no contribution
#' (0) and any contribution (1).
#'
#' @docType data
#' @keywords datasets
#' @name mpn_senate112
#' @usage data(mpn_DemSxP)
#' @format Matrix of 51 rows (Senators) and 63 columns (PACS)
#' @source Knoke, Diani, Hollway, and Christopoulos. 2021. \emph{Multimodal Political Networks}. Cambridge University Press: Cambridge.
"mpn_DemSxP"

#' @rdname mpn_senate112
#' @usage data(mpn_RepSxP)
#' @format Matrix of 62 rows (Senators) and 72 columns (PACS)
"mpn_RepSxP"

#' @rdname mpn_senate112
#' @usage data(mpn_OverSxP)
#' @format Matrix of 20 rows (Senators) and 32 columns (PACS)
"mpn_OverSxP"

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

