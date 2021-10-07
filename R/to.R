#' Tools for reformatting networks, graphs, and matrices 
#' @name to
#' @param object A matrix, `{igraph}` graph, `{tidygraph}` tbl_graph, or `{network}` object.
#' @param threshold For a matrix, the threshold to binarise/dichotomise at.
#' @examples
#' to_unweighted(project_rows(southern_women))
#' @export
to_unweighted <- function(object, threshold = 1) UseMethod("to_unweighted")

#' @export
to_unweighted.tbl_graph <- function(object, threshold = 1){
    out <- igraph::delete_edge_attr(object, "weight")
    tidygraph::as_tbl_graph(out)
}

#' @export
to_unweighted.igraph <- function(object, threshold = 1){
    if("weight" %in% igraph::edge_attr_names(object)){
      igraph::delete_edge_attr(object, "weight")
    } else object
}

#' @export
to_unweighted.network <- function(object, threshold = 1){
    out <- as_igraph(object)
    out <- igraph::delete_edge_attr(out, "weight")
    as_network(out)
}

#' @export
to_unweighted.matrix <- function(object, threshold = 1){
  object <- (object >= threshold)*1
  object
}

#' @rdname to
#' @examples
#' to_unnamed(project_rows(southern_women))
#' @export
to_unnamed <- function(object) UseMethod("to_unnamed")

#' @export
to_unnamed.igraph <- function(object){
  if("name" %in% igraph::vertex_attr_names(object)){
    igraph::delete_vertex_attr(object, "name")
  } else object
}

#' @export
to_unnamed.tbl_graph <- function(object){
    out <- igraph::delete_vertex_attr(object, "name")
    tidygraph::as_tbl_graph(out)
}

#' @export
to_unnamed.network <- function(object){
  out <- as_igraph(object)
  out <- igraph::delete_vertex_attr(out, "name")
  as_network(out)
}

#' @export
to_unnamed.matrix <- function(object){
  out <- object
  rownames(out) <- NULL
  colnames(out) <- NULL
  out
}

#' @rdname to
#' @examples
#' to_undirected(adolescent_society)
#' @export
to_undirected <- function(object) UseMethod("to_undirected")

#' @importFrom igraph as.undirected
#' @export
to_undirected.igraph <- function(object){
  igraph::as.undirected(object)
}

#' @importFrom igraph as.undirected
#' @export
to_undirected.tbl_graph <- function(object){
  as_tidygraph(igraph::as.undirected(object))
}

#' @importFrom sna symmetrize
#' @export
to_undirected.network <- function(object){
  sna::symmetrize(object)
}

#' @export
to_undirected.matrix <- function(object){
  if(is_twomode(object)){
    object
  } else ((object + t(object))>0)*1
}

