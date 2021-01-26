#' Converts objects
#' 
#' This function takes a data frame,
#' either as a data frame version of a matrix
#' or as an edgelist,
#' and returns an incidence matrix.
#' @name convert
#' @param object A data frame containing an edgelist or 
#' dataframe version of a matrix.
#' 
#' If the data frame is a 2 column edgelist,
#' the first column will become the rows
#' and the second column will become the columns.
#' 
#' If the data frame is a 3 column edgelist,
#' then the third column will be used as 
#' the cell values or tie weights.
#' 
#' If the data frame is more than 3 columns,
#' the first column is full of character strings (i.e. is named)
#' and the second column is numeric (e.g. 0 and 1)
#' then it will be assumed that this is a matrix
#' embedded in a data frame.
#' @examples
#' test <- data.frame(id1 = c("A","B","B","C","C"),
#'                    id2 = c("I","G","I","G","H"))
#' as_matrix(test)
#' @return An incidence matrix, named if possible.
#' @export
as_matrix <- function(object){
  
  if(missing(object)){
    expect_nodes()
    graph <- .G()
    if (is_bipartite(graph)){
      mat <- igraph::as_incidence_matrix(graph)
    } else {
      mat <- igraph::as_adjacency_matrix(graph)
    }
  } else if (is.igraph(object)) {
    if (is_bipartite(object)){
      mat <- igraph::as_incidence_matrix(object)
    } else {
      mat <- igraph::as_adjacency_matrix(object)
    }
  } else if (is.matrix(object)) {
    mat <- object
  } else if (is.data.frame(object)){
    if(is.character(object[,1]) & ncol(object)>2 & is.numeric(object[1,2])){
      out <- object
      row.names(out) <- out[,1]
      out[,1] <- NULL
      out <- as.matrix(out)
    } else {
      if (ncol(object)==2) {
        object <- as.data.frame(table(object[,1], object[,2]))
      }
      if (ncol(object)==3) {
        nodes1 <- as.character(unique(object[,1]))
        nodes2 <- as.character(unique(object[,2]))
        out <- structure(as.numeric(object[,3]), 
                         .Dim = c(as.integer(length(nodes1)), as.integer(length(nodes2))), 
                         .Dimnames = list(nodes1, nodes2))
      }
    }
    mat <- out
  }
  mat
}

#' @rdname convert
#' @export
as_igraph <- function(object){
  if(missing(object)){
    expect_nodes()
    graph <- .G()
    weights <- rlang::enquo(weights)
    weights <- rlang::eval_tidy(weights, .E())
  } else if (is.igraph(object)) {
    graph <- object
  } else if (is.matrix(object)) {
    graph <- igraph::graph_from_incidence_matrix(object)
  }
  graph
}
