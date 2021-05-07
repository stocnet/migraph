#' Tools for reformatting networks, graphs, and matrices 
#' @name to
#' @param object A matrix, `{igraph}` graph, `{tidygraph}` tbl_graph, or `{network}` object.
#' @param threshold For a matrix, the threshold to binarise/dichotomise at.
#' @examples
#' to_unweighted(project_rows(southern_women))
#' @export
to_unweighted <- function(object, threshold = 1){
  if(is.tbl_graph(object)){
    out <- igraph::delete_edge_attr(object, "weight")
    out <- tidygraph::as_tbl_graph(out)
  } else if(is.igraph(object)){
    if("weight" %in% igraph::edge_attr_names(object)){
      out <- igraph::delete_edge_attr(object, "weight")
    } else {
      out <- object
    }
  } else if(is.network(object)){
    out <- as_igraph(object)
    out <- igraph::delete_edge_attr(out, "weight")
    out <- as_network(out)
  } else if(is.matrix(object)){
    object <- (object >= threshold)*1
    out <- object
  } else stop("Object needs to be a tidygraph, igraph or matrix object.")
  out
}

#' @rdname to
#' @examples
#' to_unnamed(project_rows(southern_women))
#' @export
to_unnamed <- function(object){
  if(is.tbl_graph(object)){
    out <- igraph::delete_edge_attr(object, "name")
    out <- tidygraph::as_tbl_graph(out)
  } else if(is.igraph(object)){
    if("name" %in% igraph::edge_attr_names(object)){
      out <- igraph::delete_edge_attr(object, "name")
    } else {
      out <- object
    }
  } else if(is.network(object)){
    out <- as_igraph(object)
    out <- igraph::delete_edge_attr(out, "name")
    out <- as_network(out)
  } else if(is.matrix(object)){
    out <- object
    rownames(out) <- NULL
    colnames(out) <- NULL
  } else stop("Object needs to be a tidygraph, igraph or matrix object.")
  out
}
