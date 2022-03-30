# Mexican elite ####

#' One-mode Mexican power elite database (Knoke 1990)
#'
#' This data contains the full network of 35 members of the Mexican power elite.
#' The undirected lines connecting pairs of men represent any formal, informal,
#' or organizational relation between a dyad;
#' for example, “common belonging (school, sports, business, political participation),
#' or a common interest (political power)” (Mendieta et al. 1997: 37).
#' Additional nodal attributes include their full name, place of birth, state,
#' and region (1=North, 2=Centre, 3=South, original coding added by 
#' \href{https://jameshollway.com/courses/ison/heber_post}{Frank Heber}), 
#' as well as their year of entry into politics 
#' and whether they are civilian (0) or affiliated with the military (1).
#' An additional variable "in_mpn" can be used to subset this network
#' to a network of 11 core members of the 1990s Mexican power elite (Knoke 2017),
#' three of which were successively elected presidents of Mexico:
#' José López Portillo (1976-82), Miguel de la Madrid (1982-88), and Carlos Salinas de Gortari (1988-94,
#' who was also the son of another core member, Raúl Salinas Lozano).
#'
#' @docType data
#' @keywords datasets
#' @name mpn_elite_mex
#' @usage data(mpn_elite_mex)
#' @format `tbl_graph` network object. 
#'   The network is simple, undirected, and named. 
#'   The full network contains 35 nodes and 117 edges,
#'   and the subsetted network contains 11 nodes and 44 edges.
#' @source Knoke, David. 1990. \emph{Political Networks}.
#' 
#' Knoke, David, Mario Diani, James Hollway, and Dimitris C Christopoulos. 2021.
#' \href{https://www.cambridge.org/core/books/multimodal-political-networks/43EE8C192A1B0DCD65B4D9B9A7842128}{\emph{Multimodal Political Networks}}.
#' Cambridge University Press. Cambridge University Press.
"mpn_elite_mex"

# US elite ####

#' Two-mode and three-mode American power elite database (Domhoff 2016)
#'
#' @description
#' `mpn_elite_usa_advice` is a 2-mode network of persons serving as directors or trustees of think tanks.
#' Think tanks are “public-policy research analysis and engagement organizations
#' that generate policy-oriented research, analysis, and advice on domestic and international issues,
#' thereby enabling policymakers and the public to make informed decisions about public policy” (McGann 2016: 6).
#' The Power Elite Database (Domhoff 2016) includes information on the directors of 33 prominent think tanks in 2012.
#' Here we include only 14 directors who held three or more seats among 20 think tanks.
#'
#' `mpn_elite_usa_money` is based on 26 elites who sat on the boards of directors 
#' for at least two of six economic policy making organizations (Domhoff 2016),
#' and also made campaign contributions to one or more of six candidates 
#' running in the primary election contests for the 2008 Presidential nominations 
#' of the Republican Party (Rudy Giuliani, John McCain, Mitt Romney) 
#' or the Democratic Party (Hillary Clinton, Christopher Dodd, Barack Obama).
#'
#' @docType data
#' @keywords datasets
#' @name mpn_elite_usa
#' @usage data(mpn_elite_usa_advice)
#' @format `mpn_elite_usa_advice` is a two-mode, named, and unweighted `tbl_graph` 
#'   with 32 nodes and 46 edges.
#' @references
#' Domhoff, G William. 2016. \href{https://whorulesamerica.ucsc.edu/power_elite/}{“Who Rules America? Power Elite Database.”}
#' 
#' The Center for Responsive Politics. 2019. \href{http://www.opensecrets.org}{“OpenSecrets.”}
#' 
#' Knoke, David, Mario Diani, James Hollway, and Dimitris C Christopoulos. 2021.
#' \href{https://www.cambridge.org/core/books/multimodal-political-networks/43EE8C192A1B0DCD65B4D9B9A7842128}{\emph{Multimodal Political Networks}}.
#' Cambridge University Press. Cambridge University Press.
"mpn_elite_usa_advice"

#' @docType data
#' @keywords datasets
#' @rdname mpn_elite_usa
#' @usage data(mpn_elite_usa_money)
#' @format `mpn_elite_usa_advice` is a two-mode, named, and unweighted `tbl_graph` 
#'   with 38 nodes and 103 edges.
"mpn_elite_usa_money"


# Bristol ####

#' Multimodal (3) Bristol protest events, 1990-2002 (Diani and Bison 2004)
#'
#' A multimodal (3) network containing individuals affiliations to civic organizations
#' in Bristol and their participation in major protest and civic events between 1990-2002,
#' and the involvement of the organizations in these events.
#'
#' @docType data
#' @keywords datasets
#' @name mpn_bristol
#' @usage data(mpn_bristol)
#' @format A `tbl_graph` object with 264 rows and columns. Node IDs are 
#' prefaced with a type identifier:
#' \describe{
#'   \item{1_}{150 Individuals, anonymised}
#'   \item{2_}{97 Bristol Civic Organizations}
#'   \item{3_}{17 Major Protest and Civic Events in Bristol, 1990-2002}
#' }
#' The network is weighted, named, and directed.
#' @references 
#' Diani, Mario, and Ivano Bison. 2004. 
#' “Organizations, Coalitions, and Movements.” 
#' Theory and Society 33(3–4):281–309.
#' @source Knoke, David, Mario Diani, James Hollway, and Dimitris C Christopoulos. 2021.
#' \href{https://www.cambridge.org/core/books/multimodal-political-networks/43EE8C192A1B0DCD65B4D9B9A7842128}{\emph{Multimodal Political Networks}}.
#' Cambridge University Press. Cambridge University Press.
"mpn_bristol"

# Ryanair ####

#' One-mode EU policy influence network, June 2004 (Christopoulos 2006)
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
#' @format `tbl_graph` network object. The network is simple, directed, named
#' and weighted. It contains 20 nodes and 177 edges.
#' @source Christopoulos, Dimitrios C. 2006.
#' “Relational Attributes of Political Entrepreneurs: a Network Perspective.”
#' \emph{Journal of European Public Policy} 13 (5): 757–78.
#' 
#' Knoke, Diani, Hollway, and Christopoulos. 2021. 
#' \emph{Multimodal Political Networks}. Cambridge University Press: Cambridge.
"mpn_ryanair"

# Senate voting ####

#' Two-mode 112th Congress Senate Voting (Knoke et al. 2021)
#'
#' @description
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
#' @format `tbl_graph` network object. It is a bimodal, directed, named,
#' weighted graph of 51 Senators (`type = FALSE`) and 63 PACS (`type = TRUE`)
#' and 2791 edges.
#' @source Knoke, Diani, Hollway, and Christopoulos. 2021. 
#' \emph{Multimodal Political Networks}. Cambridge University Press: Cambridge.
"mpn_DemSxP"

#' @rdname mpn_senate112
#' @usage data(mpn_RepSxP)
#' @format `tbl_graph` network object. It is a bimodal, directed, named,
#' weighted graph of 62 Senators (`type = FALSE`) and 72 PACS (`type = TRUE`)
#' and 3675 edges.
"mpn_RepSxP"

#' @rdname mpn_senate112
#' @usage data(mpn_OverSxP)
#' @format `tbl_graph` network object. It is a bimodal, directed, named,
#' weighted graph of 20 Senators (`type = FALSE`) and 32 PACS (`type = TRUE`)
#' and 614 edges.
"mpn_OverSxP"

# EVS ####

#' Two-mode European Values Survey, 1990 and 2008 (EVS 2020)
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
#' @format `tbl_graph` object based on an association matrix with 14 columns:
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
#' @source Knoke, Diani, Hollway, and Christopoulos. 2021. 
#' @references 
#'   EVS (2020). European Values Study Longitudinal Data File 1981-2008 (EVS 1981-2008). 
#'   GESIS Data Archive, Cologne. ZA4804 Data file Version 3.1.0, 
#'   https://doi.org/10.4232/1.13486.
#' \emph{Multimodal Political Networks}. Cambridge University Press: Cambridge.
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

