# Features ####

#' Marking networks based on their properties
#' 
#' These functions implement logical tests for various network
#' properties.
#' @param .data An object of a `{manynet}`-consistent class:
#'   \itemize{
#'   \item matrix (adjacency or incidence) from `{base}` R
#'   \item edgelist, a data frame from `{base}` R or tibble from `{tibble}`
#'   \item igraph, from the `{igraph}` package
#'   \item network, from the `{network}` package
#'   \item tbl_graph, from the `{tidygraph}` package
#'   }
#' @return TRUE if the condition is met, or FALSE otherwise.
#' @importFrom manynet as_igraph is_directed is_twomode to_matching
#' @family marks
#' @name is
NULL

#' @describeIn is Tests whether network is weakly connected if
#'   the network is undirected or strongly connected if directed.
#'   To test weak connection on a directed network,
#'   please see `manynet::to_undirected()`.
#' @importFrom igraph is.connected
#' @examples
#' is_connected(ison_southern_women)
#' @export
is_connected <- function(.data) {
  igraph::is.connected(manynet::as_igraph(.data), 
                       mode = ifelse(manynet::is_directed(.data),
                                     "strong", "weak"))
}

#' @describeIn is Tests whether there is a matching for a network
#'   that covers every node in the network
#' @param mark A logical vector marking two types or modes.
#'   By default "type".
#' @examples
#' is_perfect_matching(ison_southern_women)
#' @export
is_perfect_matching <- function(.data, mark = "type"){
  matches <- manynet::to_matching(.data, mark = mark)
  manynet::network_ties(matches)*2 == manynet::network_nodes(matches)
}

#' @describeIn is Tests whether there is a Eulerian path for a network
#'   where that path passes through every tie exactly once
#'   @importFrom igraph has_eulerian_path
#' @examples
#' is_eulerian(ison_brandes)
#' @export
is_eulerian <- function(.data){
  igraph::has_eulerian_path(manynet::as_igraph(.data))
}

#' @describeIn is Tests whether network is a directed acyclic graph
#' @importFrom igraph is_dag
#' @examples 
#' is_acyclic(ison_algebra)
#' @export
is_acyclic <- function(.data){
  obj <- manynet::as_igraph(.data)
  igraph::is_dag(obj)
}

#' @describeIn is Tests whether network is aperiodic
#' @param max_path_length Maximum path length considered.
#'   If negative, paths of all lengths are considered.
#'   By default 4, to avoid potentially very long computation times.
#' @source https://stackoverflow.com/questions/55091438/r-igraph-find-all-cycles
#' @examples 
#' is_aperiodic(ison_algebra)
#' @export
is_aperiodic <- function(.data, max_path_length = 4){
  g <- manynet::as_igraph(.data)
  out <- NULL
  for(v1 in igraph::V(g)) {
    if(igraph::degree(g, v1, mode="in") == 0) {next}
    goodNeighbors <- igraph::neighbors(g, v1, mode="out")
    goodNeighbors <- goodNeighbors[goodNeighbors > v1]
    out <- c(out, unlist(lapply(goodNeighbors, function(v2){
      vapply(igraph::all_simple_paths(g, v2, v1, mode="out", 
                                      cutoff = max_path_length), length, FUN.VALUE = numeric(1))
    })))
  }
  if (!("minMSE" %in% rownames(utils::installed.packages()))) {
    message("Please install package `{minMSE}` from CRAN.")
  } else {
    minMSE::vector_gcd(out)==1
  }
}
