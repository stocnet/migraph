expect_nodes <- function() {
  if (!.graph_context$free() && .graph_context$active() != 'nodes') {
    stop('This call requires nodes to be active', call. = FALSE)
  }
}

expect_edges <- function() {
  if (!.graph_context$free() && .graph_context$active() != 'edges') {
    stop('This call requires edges to be active', call. = FALSE)
  }
}
