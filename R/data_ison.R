#' Multilevel two-mode affiliation, signed one-mode networks of Marvel comic book characters
#'
#' @details
#' This package includes two datasets related to the Marvel _comic book_ universe.
#' The first, `ison_marvel_teams`,  is a two-mode affiliation network of 53 Marvel comic book characters and their
#' affiliations to 141 different teams.
#' This network includes only information about nodes' names and nodeset,
#' but additional nodal data can be taken from the other Marvel dataset here.
#' 
#' The second network, `ison_marvel_relationships`, is a one-mode signed network
#' of friendships and enmities between the 53 Marvel comic book characters.
#' Friendships are indicated by a positive sign in the edge `sign` attribute,
#' whereas enmities are indicated by a negative sign in this edge attribute.
#' Additional nodal variables have been coded and included by Dr Umut Yüksel:
#' 
#' - **Gender**: binary character, 43 "Male" and 10 "Female"
#' - **PowerOrigin**: binary character, 2 "Alien", 1 "Cyborg", 5 "God/Eternal",
#' 22 "Human", 1 "Infection", 16 "Mutant", 5 "Radiation", 1 "Robot"
#' - **Appearances**: integer, in how many comic book issues they appeared in
#' - **Attractive**: binary integer, 41 1 (yes) and 12 0 (no)
#' - **Rich**: binary integer, 11 1 (yes) and 42 0 (no)
#' - **Intellect**: binary integer, 39 1 (yes) and 14 0 (no)
#' - **Omnilingual**: binary integer, 8 1 (yes) and 45 0 (no)
#' - **UnarmedCombat**: binary integer, 51 1 (yes) and 2 0 (no)
#' - **ArmedCombat**: binary integer, 25 1 (yes) and 28 0 (no)
#' @docType data
#' @keywords datasets
#' @name ison_marvel
#' @usage data(ison_marvel_teams)
#' @source Umut Yüksel, 31 March 2017
#' @format Two-mode igraph of 53 Marvel comic book characters and 141 team-ups, with 683 team affiliations between them
"ison_marvel_teams"

#' @rdname ison_marvel
#' @usage data(ison_marvel_relationships)
#' @format One-mode igraph of 53 Marvel comic book characters and 558 signed (`1` = friends, `-1` = enemies) undirected ties
"ison_marvel_relationships"

#' Two-mode projection examples
#'
#' @docType data
#' @keywords datasets
#' @name ison_projection
#' @usage data(ison_mm)
#' @format Directed two-mode igraph with 6 nodes and 6 edges
"ison_mm"

#' @rdname ison_projection
#' @usage data(ison_bm)
#' @format Directed two-mode igraph with 8 nodes and 9 edges
"ison_bm"

#' @rdname ison_projection
#' @usage data(ison_mb)
#' @format Directed two-mode igraph with 8 nodes and 9 edges
"ison_mb"

#' @rdname ison_projection
#' @usage data(ison_bb)
#' @format Directed two-mode igraph with 10 nodes and 12 edges
"ison_bb"

#' Multiplex igraph of friends, social, and task ties between 16 anonymous students
#' 
#' M182 was an honors algebra class and friendship, social, and task ties
#' were collected/observed.
#' @docType data
#' @keywords datasets
#' @name m182
#' @usage data(ison_m182)
#' @source See also `data(studentnets.M182, package = "NetData")`
#' Larger comprehensive data set publicly available, contact Daniel A. McFarland for details.
#' @format Multiplex tidygraph of friends, social, and task ties between 16 anonymous students
#' The edge attribute `friend_ties` contains friendship ties,
#' where `2` = best friends, `1` = friend, and `0` is not a friend.
#' `social_ties` consists of social interactions per hour,
#' and `task_ties` consists of task interactions per hour.
"ison_m182"

#' One-mode subset of adolescent society dataset
#'
#' @docType data
#' @keywords datasets
#' @name ison_coleman
#' @usage data(ison_coleman)
#' @format tidygraph graph object
#' @references Coleman, James S. 1961. The Adolescent Society.
#' New York:Free Press.
#' 
#' Feld, Scott. 1991. “Why your friends have more friends than you do”
#' American Journal of Sociology 96(6): 1464-1477.
"ison_coleman"

#' Zachary's kareteka network
#'
#' @docType data
#' @keywords datasets
#' @name ison_community
#' @usage data(ison_karateka)
#' @format Undirected one-mode igraph with 34 nodes and 78 edges
"ison_karateka"

#' 1DW 32(2) 440(1) EIES messages
#'
#' @docType data
#' @keywords datasets
#' @name ison_eies
#' @usage data(ison_eies)
#' @format tidygraph graph object
#' @source networkdata package
#' @references Freeman, S. C. and L. C. Freeman (1979). 
#' The networkers network: A study of the impact of a new communications medium on sociometric structure. 
#' Social Science Research Reports No 46. Irvine CA, University of California.
#' 
#' Wasserman S. and K. Faust (1994). 
#' Social Network Analysis: Methods and Applications.
#' Cambridge University Press, Cambridge.
"ison_eies"

