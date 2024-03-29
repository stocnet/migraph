#' @importFrom igraph is_igraph
#' @export
igraph::is_igraph

#' @importFrom network is.network
#' @export
network::is.network

#' @importFrom network as.network
#' @export
network::as.network

#' @importFrom dplyr %>%
#' @export
dplyr::`%>%`

#' @importFrom tidygraph with_graph
#' @export
tidygraph::with_graph

#' @importFrom tidygraph is.tbl_graph
#' @export
tidygraph::is.tbl_graph

#' @importFrom tidygraph .G
#' @export
tidygraph::.G

#' @importFrom tidygraph .N
#' @export
tidygraph::.N

#' @importFrom tidygraph .E
#' @export
tidygraph::.E

#' @importFrom tidygraph mutate
#' @export
tidygraph::mutate

#' @importFrom tidygraph rename
#' @export
tidygraph::rename

#' @importFrom tidygraph bind_edges
#' @export
tidygraph::bind_edges

expect_nodes <- function() {
  if (!.graph_context$free() && .graph_context$active() != "nodes") {
    stop("This call requires nodes to be active", call. = FALSE)
  }
}

expect_edges <- function() {
  if (!.graph_context$free() && .graph_context$active() != "edges") {
    stop("This call requires edges to be active", call. = FALSE)
  }
}

# #' @importFrom rlang `%||%`
