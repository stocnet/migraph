#' Coercion between graph/network object classes 
#' 
#' The `as_` functions in `{migraph}` coerce objects
#' between several common classes of social network objects.
#' These include:
#' - adjacency and incidence matrices
#' - edgelists (as data frames)
#' - `{igraph}` `graph` objects
#' - `{tidygraph}` `tbl_graph` objects
#' - `{network}` `network` objects
#' @name coercion
#' @param object A data frame edgelist, matrix, igraph, tidygraph, or
#' network object.
#' @param twomode An option to override the heuristics for distinguishing
#' incidence from adjacency matrices. By default FALSE.
#' @param weight An option to override the heuristics for distinguishing
#' weighted networks. By default FALSE.
#' @details Behaviour is a little different depending on the data format.
#' 
#' If the data frame is a 2 column edgelist,
#' the first column will become the rows
#' and the second column will become the columns.
#' If the data frame is a 3 column edgelist,
#' then the third column will be used as 
#' the cell values or tie weights.
#' 
#' Incidence matrices are typically inferred from unequal dimensions,
#' but since in rare cases a matrix with equal dimensions may still
#' be an incidence matrix, an additional argument `twomode` can be
#' specified to override this heuristic.
#' This information is usually already embedded in `{igraph}`, 
#' `{tidygraph}`, and `{network}` objects.
#' @importFrom tidygraph as_tbl_graph is.tbl_graph
#' @importFrom network is.network as.network
#' @importFrom network as.matrix.network.incidence as.matrix.network.adjacency
#' @examples
#' test <- data.frame(id1 = c("A","B","B","C","C"),
#'                    id2 = c("I","G","I","G","H"))
#' as_matrix(test)
#' as_igraph(test)
#' as_tidygraph(test)
#' as_network(test)
#' @return
#' The currently implemented coercions or translations are:
#' 
#' |  to/from      | edgelists           | matrices  |igraph  |tidygraph  |network  |
#' | ------------- |:-----:|:-----:|:-----:|:-----:|:-----:|
#' | edgelists (data frames)  |  |  |  |  |  |
#' | matrices                 | X | X | X | X | X |
#' | igraph                   | X | X | X | X | X |
#' | tidygraph                | X | X | X | X | X |
#' | network                  | X | X | X | X | X |
#' @export
as_matrix <- function(object, weight = FALSE) UseMethod("as_matrix")

#' @export
as_matrix.data.frame <- function(object, weight = FALSE){
      if (ncol(object) == 2 | !weight) {
        object <- data.frame(object) # in case it's a tibble
        object <- as.data.frame(table(c(object[,1]),
                                      c(object[,2])))
      }
      if (ncol(object) == 3) {
        # Adds a third (weight) column to a two-column edgelist
        # object <- object[order(object[,1], object[,2]),]
        nodes1 <- as.character(unique(object[,1]))
        nodes1 <- sort(nodes1)
        nodes2 <- as.character(unique(object[,2]))
        nodes2 <- sort(nodes2)
        if (nrow(object) != length(nodes1)*length(nodes2)) {
          allcombs <- expand.grid(object[,1:2], stringsAsFactors = FALSE)
          allcombs <- subset(allcombs, !duplicated(allcombs))
          object <- merge(allcombs, object, all.x = TRUE)
          object <- object[order(object[,2], object[,1]),]
          object[is.na(object)] <- 0
        }
        out <- structure(as.numeric(object[,3]),
                         .Dim = c(as.integer(length(nodes1)),
                                  as.integer(length(nodes2))),
                         .Dimnames = list(nodes1, nodes2))
      }
  out
}

#' @export
as_matrix.matrix <- function(object, weight = FALSE) {
  object
}

#' @export
as_matrix.igraph <- function(object, weight = FALSE) {
      if (is_twomode(object)) {
        mat <- igraph::as_incidence_matrix(object, sparse = FALSE)
      } else {
        mat <- igraph::as_adjacency_matrix(object, sparse = FALSE)
      }
}

#' @export
as_matrix.tbl_graph <- function(object, weight = FALSE) {
  as_matrix(as_igraph(object))
}

#' @export
as_matrix.network <- function(object, weight = FALSE) {
  network::as.matrix.network(object)
}

#' @rdname coercion
#' @importFrom igraph graph_from_data_frame graph_from_incidence_matrix
#' @importFrom igraph graph_from_adjacency_matrix
#' @export
as_igraph <- function(object,
                      weight = FALSE,
                      twomode = FALSE) UseMethod("as_igraph")

#' @export
as_igraph.data.frame <- function(object,
                                 weight = FALSE,
                                 twomode = FALSE) {
  graph <- igraph::graph_from_data_frame(object)
  if(length(intersect(c(object[,1]), c(object[,2]))) == 0){
    igraph::V(graph)$type <- igraph::V(graph)$name %in% object[,2]
  }
  graph
}

#' @export
as_igraph.matrix <- function(object,
                             weight = FALSE,
                             twomode = FALSE) {
  if (nrow(object) != ncol(object) | twomode) {
    graph <- igraph::graph_from_incidence_matrix(object)
  } else {
    graph <- igraph::graph_from_adjacency_matrix(object)
  }
  graph
}

#' @export
as_igraph.igraph <- function(object,
                             weight = FALSE,
                             twomode = FALSE) {
  class(object) <- "igraph"
  object
}

#' @export
as_igraph.tbl_graph <- function(object,
                                weight = FALSE,
                                twomode = FALSE) {
  class(object) <- "igraph"
  object
}

#' @export
as_igraph.network <- function(object, weight = FALSE, 
                                twomode = FALSE) {
  if (network::is.bipartite(object)) {
    graph <- network::as.matrix.network.incidence(object)
    graph <- igraph::graph_from_incidence_matrix(graph)
  } else {
    graph <- network::as.matrix.network.adjacency(object)
    graph <- igraph::graph_from_adjacency_matrix(graph)
  }
  graph
}

#' @rdname coercion
#' @export
as_tidygraph <- function(object, twomode = FALSE) UseMethod("as_tidygraph")

#' @export
as_tidygraph.data.frame <- function(object, twomode = FALSE) {
  tidygraph::as_tbl_graph(as_igraph(object))
}

#' @export
as_tidygraph.matrix <- function(object, twomode = FALSE) {
  tidygraph::as_tbl_graph(as_igraph(object))
}

#' @export
as_tidygraph.igraph <- function(object, twomode = FALSE) {
  tidygraph::as_tbl_graph(object)
}

#' @export
as_tidygraph.tbl_graph <- function(object, twomode = FALSE) {
  object
}

#' @export
as_tidygraph.network <- function(object, twomode = FALSE) {
  tidygraph::as_tbl_graph(as_igraph(object))
}

#' @rdname coercion
#' @export
as_network <- function(object) UseMethod("as_network")

#' @export
as_network.network <- function(object) {
  object
}

#' @export
as_network.matrix <- function(object) {
  if (is_twomode(object)) {
    network::as.network(object, bipartite = TRUE)
  } else {
    network::as.network(object, bipartite = FALSE)
  }
}

#' @export
as_network.igraph <- function(object) {
  as_network(as_matrix(object))
}

#' @export
as_network.tbl_graph <- function(object) {
  as_network(as_matrix(object))
}

#' @export
as_network.data.frame <- function(object) {
  as_network(as_matrix(object))
}
