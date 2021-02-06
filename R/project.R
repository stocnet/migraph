#' Projecting two-mode objects into one-mode objects
#' 
#' These functions 'project' or convert a two-mode object
#' in any format -- tidygraph, igraph, or matrix --
#' into a corresponding one-mode object.
#' 
#' `project_rows()` results in a weighted one-mode object
#' that retains the row nodes from a two-mode object,
#' and weights the ties between them on the basis of
#' their joint ties to nodes in the second mode (columns).
#' 
#' `project_cols()` results in a weighted one-mode object
#' that retains the column nodes from a two-mode object,
#' and weights the ties between them on the basis of
#' their joint ties to nodes in the first mode (rows).
#' @name project 
#' @param object A matrix, `igraph` graph or `tidygraph` tbl_graph object.
#' @importFrom igraph bipartite.projection
#' @importFrom tidygraph as_tbl_graph
#' @examples
#' project_rows(southern_women)
#' @export
project_rows <- function(object){
  if(is.tbl_graph(object)){
    out <- igraph::bipartite.projection(object)$proj1
    out <- tidygraph::as_tbl_graph(out)
  } else if(is.igraph(object)){
    out <- igraph::bipartite.projection(object)$proj1
  } else if(is.matrix(object)){
    out <- object %*% t(object)
  } else stop("Object needs to be a tidygraph, igraph or matrix object.")
  out
}

#' @rdname project
#' @examples
#' project_cols(southern_women)
#' @export
project_cols <- function(object){
  if(is.tbl_graph(object)){
    out <- igraph::bipartite.projection(object)$proj2
    out <- tidygraph::as_tbl_graph(out)
  } else if(is.igraph(object)){
    out <- igraph::bipartite.projection(object)$proj2
  } else if(is.matrix(object)){
    out <- t(object) %*% object
  } else stop("Object needs to be a tidygraph, igraph or matrix object.")
  out
}