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
#'   ```{r, eval = FALSE, echo = FALSE, include = FALSE}
#'   autographr(mpn_elite_mex, labels = FALSE) + ggtitle("mpn_elite_mex")
#'   ggsave("man/figures/mpnelitemex.png", width = 5, height = 5, dpi = "screen")
#'   ggsave("man/figures/mpnelitemex.pdf", width = 5, height = 5)
#'   ```
#' \if{html}{\figure{mpnelitemex.png}{options: width="100\%" alt="Figure: mpn_elite_mex"}}
#' \if{latex}{\figure{mpnelitemex.pdf}{options: width=7cm}}
#' @docType data
#' @keywords datasets
#' @name mpn_elite_mex
#' @usage data(mpn_elite_mex)
#' @source Knoke, David. 1990. \emph{Political Networks}.
#' 
#' Knoke, David, Mario Diani, James Hollway, and Dimitris C Christopoulos. 2021.
#' \href{https://www.cambridge.org/core/books/multimodal-political-networks/43EE8C192A1B0DCD65B4D9B9A7842128}{\emph{Multimodal Political Networks}}.
#' Cambridge University Press. Cambridge University Press.
#' @format 
#'   ```{r, echo = FALSE}
#'   mpn_elite_mex
#'   ```
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
#' @details 
#'   ```{r, eval = FALSE, echo = FALSE, include = FALSE}
#'   autographr(mpn_elite_usa_advice, labels = FALSE) + ggtitle("mpn_elite_usa_advice") +
#'     autographr(mpn_elite_usa_money, labels = FALSE) + ggtitle("mpn_elite_usa_money")
#'   ggsave("man/figures/mpneliteusa.png", width = 7, height = 3.5, dpi = "screen")
#'   ggsave("man/figures/mpneliteusa.pdf", width = 7, height = 3.5)
#'   ```
#' \if{html}{\figure{mpneliteusa.png}{options: width="100\%" alt="Figure: mpn_elite_usa"}}
#' \if{latex}{\figure{mpneliteusa.pdf}{options: width=7cm}}
#' @docType data
#' @keywords datasets
#' @name mpn_elite_usa
#' @usage data(mpn_elite_usa_advice)
#' @format 
#'   ```{r, echo = FALSE}
#'   mpn_elite_usa_advice
#'   ```
#' @references
#' Domhoff, G William. 2016. \href{https://whorulesamerica.ucsc.edu/power_elite/}{“Who Rules America? Power Elite Database.”}
#' 
#' The Center for Responsive Politics. 2019. \href{https://www.opensecrets.org}{“OpenSecrets.”}
#' 
#' Knoke, David, Mario Diani, James Hollway, and Dimitris C Christopoulos. 2021.
#' \href{https://www.cambridge.org/core/books/multimodal-political-networks/43EE8C192A1B0DCD65B4D9B9A7842128}{\emph{Multimodal Political Networks}}.
#' Cambridge University Press. Cambridge University Press.
"mpn_elite_usa_advice"

#' @docType data
#' @keywords datasets
#' @rdname mpn_elite_usa
#' @usage data(mpn_elite_usa_money)
#' @format 
#'   ```{r, echo = FALSE}
#'   mpn_elite_usa_money
#'   ```
"mpn_elite_usa_money"


# Bristol ####

#' Multimodal (3) Bristol protest events, 1990-2002 (Diani and Bison 2004)
#'
#' A multimodal network with three levels representing ties between individuals,
#' civic organisations in Bristol, and major protest and civic events that occurred 
#' between 1990 and 2000. The data contains individuals' affiliations to civic organizations
#' in Bristol, the participation of these individuals in major protest and civic events between 1990-2002,
#' and the involvement of the civic organizations in these events.
#'
#' @docType data
#' @keywords datasets
#' @name mpn_bristol
#' @usage data(mpn_bristol)
#' @details 
#' Although represented as a two-mode network, it contains three levels:
#' \describe{
#'   \item{1.}{150 Individuals, anonymised with numeric ID}
#'   \item{2.}{97 Bristol civic organizations}
#'   \item{3.}{17 Major protest and civic events in Bristol, 1990-2002}
#' }
#' The network represents ties between level 1 (individuals) and level 2 (organisations), 
#' level 1 (individuals) and level 3 (events), as well as level 2 (organisations)
#' and level 3 (events). The network is simple, undirected, and named. For a complete list of 
#' civic organisations and protest/civic events included in the data, please see Appendix 6.1
#' in \href{https://www.cambridge.org/core/books/multimodal-political-networks/43EE8C192A1B0DCD65B4D9B9A7842128}{\emph{Multimodal Political Networks}}
#' (Knoke et al., 2021).
#' @references 
#' Diani, Mario, and Ivano Bison. 2004.
#' “Organizations, Coalitions, and Movements.”
#' _Theory and Society_ 33(3–4):281–309.
#' \doi{10.1023/B:RYSO.0000038610.00045.07}.
#' @source
#' Knoke, David, Mario Diani, James Hollway, and Dimitris C Christopoulos. 2021.
#' \href{https://www.cambridge.org/core/books/multimodal-political-networks/43EE8C192A1B0DCD65B4D9B9A7842128}{\emph{Multimodal Political Networks}}.
#' Cambridge University Press. Cambridge University Press.
#' @format 
#'   ```{r, echo = FALSE}
#'   mpn_bristol
#'   ```
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
#'   ```{r, eval = FALSE, echo = FALSE, include = FALSE}
#'   autographr(mpn_ryanair, "sphere", labels = FALSE) + ggtitle("mpn_ryanair")
#'   ggsave("man/figures/mpn_ryanair.png", width = 5, height = 5, dpi = "screen")
#'   ggsave("man/figures/mpn_ryanair.pdf", width = 7, height = 3.5)
#'   ```
#' @docType data
#' @keywords datasets
#' @name mpn_ryanair
#' @usage data(mpn_ryanair)
#' @source Christopoulos, Dimitrios C. 2006.
#' “Relational Attributes of Political Entrepreneurs: a Network Perspective.”
#' \emph{Journal of European Public Policy} 13(5): 757–78.
#' \doi{10.1080/13501760600808964}.
#' 
#' Knoke, David, Mario Diani, James Hollway, and Dimitris C Christopoulos. 2021.
#' \href{https://www.cambridge.org/core/books/multimodal-political-networks/43EE8C192A1B0DCD65B4D9B9A7842128}{\emph{Multimodal Political Networks}}.
#' Cambridge University Press. Cambridge University Press.
#' @format 
#'   ```{r, echo = FALSE}
#'   mpn_ryanair
#'   ```
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
#' @docType data
#' @keywords datasets
#' @name mpn_senate112
#' @usage data(mpn_senate_dem)
#' @references  
#' Knoke, David, Mario Diani, James Hollway, and Dimitris C Christopoulos. 2021.
#' \href{https://www.cambridge.org/core/books/multimodal-political-networks/43EE8C192A1B0DCD65B4D9B9A7842128}{\emph{Multimodal Political Networks}}.
#' Cambridge University Press. Cambridge University Press.
#' @format 
#'   ```{r, echo = FALSE}
#'   mpn_senate_dem
#'   ```
"mpn_senate_dem"

#' @rdname mpn_senate112
#' @usage data(mpn_senate_rep)
#' @format 
#'   ```{r, echo = FALSE}
#'   mpn_senate_rep
#'   ```
"mpn_senate_rep"

#' @rdname mpn_senate112
#' @usage data(mpn_senate_over)
#' @format 
#'   ```{r, echo = FALSE}
#'   mpn_senate_over
#'   ```
"mpn_senate_over"

# EVS ####

#' Two-mode European Values Survey, 1990 and 2008 (EVS 2020)
#'
#' 6 two-mode matrices containing individuals' memberships to 14 different types
#' of associations in three countries (Italy, Germany, and the UK) in 1990
#' and 2008. The Italy data has 658 respondents in 1990 and 540 in 2008.
#' The Germany data has 1369 respondents in 1990 and 503 in 2008.
#' The UK data has 738 respondents in 1990 and 664 in 2008.
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
#' @source Knoke, David, Mario Diani, James Hollway, and Dimitris C Christopoulos. 2021.
#'   \href{https://www.cambridge.org/core/books/multimodal-political-networks/43EE8C192A1B0DCD65B4D9B9A7842128}{\emph{Multimodal Political Networks}}.
#'   Cambridge University Press. Cambridge University Press.
#' @references 
#'   EVS (2020). European Values Study Longitudinal Data File 1981-2008 (EVS 1981-2008).
#'   GESIS Data Archive, Cologne. ZA4804 Data file Version 3.1.0,
#'   \doi{10.4232/1.13486}.
NULL

#' @rdname mpn_evs
#' @usage data(mpn_IT_1990)
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

# COW ####

#' One-mode interstate trade relations and two-mode state membership in IGOs (COW)
#'
#' @details 
#' `mpn_cow_trade` is a one-mode matrix representing the trade relations between 116 states. 
#' The data is derived from the Correlates of War Project (COW) Trade Dataset (v3.0),
#' which contains the annual dyadic and national trade figures for states (listed in COW)
#' between 1870 to 2009.
#' This network is based only on the dyadic trade figures in 2009 for the 116 states
#' listed in Appendix 7.1 in 
#' \href{https://www.cambridge.org/core/books/multimodal-political-networks/43EE8C192A1B0DCD65B4D9B9A7842128}{\emph{Multimodal Political Networks}}
#' (Knoke et al., 2021).
#' The value in each cell of the matrix represents the value of exports
#' from the 116 row states to the 116 column states.
#' 
#' `mpn_cow_igo` is a two-mode graph representing the membership of 116 states
#' in 40 intergovernmental organizations (IGOs).
#' The data is derived from the Correlates of War Project (COW)
#' Intergovernmental Organizations Dataset (v3.0),
#' which contains information about intergovernmental organizations from 1815-2014,
#' such as founding year and membership.
#' This network contains only a subset of the states and IGOs listed in COW,
#' with 116 states listed in Appendix 7.1 in \href{https://www.cambridge.org/core/books/multimodal-political-networks/43EE8C192A1B0DCD65B4D9B9A7842128}{\emph{Multimodal Political Networks}}
#' and 40 IGOs from Table 7.1 in \href{https://www.cambridge.org/core/books/multimodal-political-networks/43EE8C192A1B0DCD65B4D9B9A7842128}{\emph{Multimodal Political Networks}}
#' that also overlap with the COW dataset (Knoke et al., 2021).
#' @name mpn_cow
#' @docType data
#' @keywords datasets
#' @usage data(mpn_cow_trade)
#' @references
#' Barbieri, Katherine, Omar M. G. Keshk, and Brian Pollins. 2009.
#' “TRADING DATA: Evaluating our Assumptions and Coding Rules.” 
#' _Conflict Management and Peace Science_ 26(5): 471-491.
#' \doi{10.1177/0738894209343887}.
#' 
#' Knoke, David, Mario Diani, James Hollway, and Dimitris C Christopoulos. 2021.
#' \href{https://www.cambridge.org/core/books/multimodal-political-networks/43EE8C192A1B0DCD65B4D9B9A7842128}{\emph{Multimodal Political Networks}}.
#' Cambridge University Press. Cambridge University Press.
#' @source 
#' The Correlates of War Project. 2012. \emph{Trade}.
#' 
#' Barbieri, Katherine and Omar Keshk. 2012. 
#' Correlates of War Project Trade Data Set Codebook, Version 3.0.
#' @format 
#'   ```{r, echo = FALSE}
#'   mpn_cow_trade
#'   ```
"mpn_cow_trade"

#' @rdname mpn_cow
#' @usage data(mpn_cow_igo)
#' @source
#' The Correlates of War Project. 2019. \emph{Intergovernmental Organization v3}.
#' @references 
#' Pevehouse, Jon C.W., Timothy Nordstron, Roseanne W McManus, Anne Spencer Jamison. 2020.
#' “Tracking Organizations in the World: The Correlates of War IGO Version 3.0 datasets”.
#' _Journal of Peace Research_ 57(3): 492-503.
#' \doi{10.1177/0022343319881175}.
#' @format 
#'   ```{r, echo = FALSE}
#'   mpn_cow_igo
#'   ```
"mpn_cow_igo"
