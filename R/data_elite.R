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
