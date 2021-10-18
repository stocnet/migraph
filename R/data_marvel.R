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

# marvel_nodes <- read.csv("/Users/hollway/Dropbox/Teaching/Courses/ISON/Data/Marvel/Attributes.csv")
# library(dplyr)
# marvel_nodes <- marvel_nodes %>% select(-Birthday) %>% rename("Attractive" = Attractive.Male.Female,
#                                                               "Rich" = Insanely.Rich) %>% as_tibble()
# marvel_affil <- as_igraph(as.matrix(read.csv("/Users/hollway/Dropbox/Teaching/Courses/ISON/Data/Marvel/Affiliation.csv", row.names = 1)))
# library(tidygraph)
# marvel_affil <- marvel_affil %>% as_tidygraph() %>% activate(nodes) %>% 
#   mutate(Gender = marvel_nodes$Gender[match(V(marvel_affil)$name, marvel_nodes$Character)],
#          PowerOrigin = marvel_nodes$Type[match(V(marvel_affil)$name, marvel_nodes$Character)],
#          Appearances = marvel_nodes$Appearances[match(V(marvel_affil)$name, marvel_nodes$Character)],
#          Attractive = marvel_nodes$Attractive[match(V(marvel_affil)$name, marvel_nodes$Character)],
#          Rich = marvel_nodes$Rich[match(V(marvel_affil)$name, marvel_nodes$Character)],
#          Intellect = marvel_nodes$Intellect[match(V(marvel_affil)$name, marvel_nodes$Character)],
#          Omnilingual = marvel_nodes$Omni.lingual[match(V(marvel_affil)$name, marvel_nodes$Character)],
#          UnarmedCombat = marvel_nodes$Unarmed.Combat[match(V(marvel_affil)$name, marvel_nodes$Character)],
#          ArmedCombat = marvel_nodes$Weapon.Master[match(V(marvel_affil)$name, marvel_nodes$Character)])
# marvel_frien <- read.csv("/Users/hollway/Dropbox/Teaching/Courses/ISON/Data/Marvel/Marvel Ties 02-13.csv")
# marvel_frien <- subset(marvel_frien, marvel_frien$Character.1 %in% marvel_nodes$Character & marvel_frien$Character.2 %in% marvel_nodes$Character)
# marvel_frien <- graph_from_data_frame(marvel_frien, directed = FALSE, vertices = marvel_nodes)
