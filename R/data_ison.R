# Marvel ####

#' Multilevel two-mode affiliation, signed one-mode networks of Marvel comic book characters (Yüksel 2017)
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

# Projection ####

#' Two-mode projection examples (Hollway 2021)
#' 
#' @details These datasets should only be used
#' for demonstration purposes as they do not describe a real world network.
#' All examples contain named nodes.
#'
#' @docType data
#' @keywords datasets
#' @name ison_projection
#' @usage data(ison_mm)
#' @format Directed two-mode `{igraph}` object with 6 nodes and 6 edges
"ison_mm"

#' @rdname ison_projection
#' @usage data(ison_bm)
#' @format Directed two-mode `{igraph}` object with 8 nodes and 9 edges
"ison_bm"

#' @rdname ison_projection
#' @usage data(ison_mb)
#' @format Directed two-mode `{igraph}` object with 8 nodes and 9 edges
"ison_mb"

#' @rdname ison_projection
#' @usage data(ison_bb)
#' @format Directed two-mode `{igraph}` object with 10 nodes and 12 edges
"ison_bb"

# Algebra ####

#' Multiplex graph object of friends, social, and task ties (McFarland 2001)
#' 
#' @details Multiplex graph object of friends, social, and task ties between 16 #' anonymous students. 
#' M182 was an honors algebra class where researchers
#' collected friendship, social, and task ties between 16 students.
#' The edge attribute `friend_ties` contains friendship ties,
#' where `2` = best friends, `1` = friend, and `0` is not a friend.
#' `social_ties` consists of social interactions per hour,
#' and `task_ties` consists of task interactions per hour.
#' @docType data
#' @keywords datasets
#' @name ison_algebra
#' @usage data(ison_algebra)
#' @references 
#' McFarland, Daniel A. (2001) ``Student Resistance.'' 
#' American Journal of Sociology, 107(3), p 612-678.
#' @source See also `data(studentnets.M182, package = "NetData")`
#' Larger comprehensive data set publicly available, contact Daniel A. McFarland for details.
#' @format Multiplex `tbl_graph` object of friends, social, and task ties 
#' between 16 anonymous students.
"ison_algebra"

# Adolescents ####

#' One-mode subset of the adolescent society (Coleman 1961)
#'
#' @docType data
#' @keywords datasets
#' @name ison_adolescents
#' @usage data(ison_adolescents)
#' @format A undirected one-mode tbl_graph object of 8 named nodes and 10
#' edges.
#' @references 
#' Coleman, James S. 1961. The Adolescent Society.
#' New York:Free Press.
#' 
#' Feld, Scott. 1991. “Why your friends have more friends than you do”
#' American Journal of Sociology 96(6): 1464-1477.
"ison_adolescents"

# Karateka ####

#' One-mode karateka network (Zachary 1977)
#' 
#' @details Zachary's karateka network. The network was observed in a 
#' university Karate club in 1977. The network describes association patterns
#' among 34 members and maps out allegiance patterns between members and either
#' Mr. Hi, the instructor, or the John A. the club president after an argument
#' about hiking the price for lessons. The allegiance of each node is 
#' listed in the `obc` argument which takes the value 1 if the individual
#' sided with Mr. Hi after the fight and 2 if the individual sided with John A.
#'
#' @docType data
#' @keywords datasets
#' @name ison_karateka
#' @usage data(ison_karateka)
#' @references 
#' Zachary, Wayne W. 1977. “An Information Flow Model for Conflict and Fission in Small Groups.” 
#' Journal of Anthropological Research 33(4):452–73.
#' @format Undirected one-mode igraph with 34 named nodes and 78 edges.
"ison_karateka"

# Networkers ####

#' One-mode EIES dataset (Freeman and Freeman 1979)
#'
#' @description A directed, simple, named, weighted graph with 32 nodes and 440
#' edges. Nodes are academics and edges illustrate the communication patterns
#' on an Electronic Information Exchange System among them. Node attributes
#' include the number of citations (`Citations`) and the discipline of the
#' researchers (`Discipline`). Edge weights illustrate the number of emails
#' sent from one academic to another over the studied time period.
#' @docType data
#' @keywords datasets
#' @name ison_networkers
#' @usage data(ison_networkers)
#' @format `tbl_graph` network object. The network is directed, simple, named
#' , and weighted. It contains 32 nodes and 440 edges as well as two node level
#' attributes: `Citations`; `Discipline`.
#' @source networkdata package
#' @references 
#' Freeman, S. C. and L. C. Freeman (1979). 
#' \emph{The networkers network: A study of the impact of a new communications medium on sociometric structure}. 
#' Social Science Research Reports No 46. Irvine CA, University of California.
#' 
#' Wasserman S. and K. Faust (1994). 
#' \emph{Social Network Analysis: Methods and Applications}.
#' Cambridge University Press, Cambridge.
"ison_networkers"

# Brandes ####

#' One-mode centrality demonstration network
#' 
#' This network should solely be used
#' for demonstration purposes as it does not describe a real network.
#'
#' @docType data
#' @keywords datasets
#' @name ison_brandes
#' @usage data(ison_brandes)
#' @format A tidygraph `tbl_graph` with 11 nodes and 24 edges.
"ison_brandes"

# Southern Women ####

#' Two-mode southern women (Davis, Gardner and Gardner 1941)
#'
#' Two-mode network dataset collected by Davis, Gardner and Gardner (1941) 
#' about the attendance pattern of women at informal social events 
#' during a 9 month period. Events and women are named.
#'
#' @docType data
#' @keywords datasets
#' @name ison_southern_women
#' @usage data(ison_southern_women)
#' @format `{igraph}` two-mode graph object with 18 women and 14 informal social
#' events.
#' @references Davis, A., Gardner, B., and Gardner, R. 1941. \emph{Deep South}.
#' Chicago: University of Chicago Press.
"ison_southern_women"
