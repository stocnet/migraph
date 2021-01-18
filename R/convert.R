#' Convert a two-mode data frame into an incidence matrix
#' 
#' This function takes a data frame,
#' either as a data frame version of a matrix
#' or as an edgelist,
#' and returns an incidence matrix.
#' @name convert
#' @param df A data frame containing an edgelist or 
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
#' as_incidence_matrix(test)
#' @return An incidence matrix, named if possible.
#' @export
as_incidence_matrix <- function(df){
  if(!is.data.frame(df)) stop("This function expects a data frame as input.")
  
  if(is.character(df[,1]) & ncol(df)>2 & is.numeric(df[1,2])){
    out <- df
    row.names(out) <- out[,1]
    out[,1] <- NULL
    out <- as.matrix(out)
  } else {
    if (ncol(df)==2) {
      df <- as.data.frame(table(df[,1], df[,2]))
    }
    if (ncol(df)==3) {
      nodes1 <- as.character(unique(df[,1]))
      nodes2 <- as.character(unique(df[,2]))
      out <- structure(as.numeric(df[,3]), 
                       .Dim = c(as.integer(length(nodes1)), as.integer(length(nodes2))), 
                       .Dimnames = list(nodes1, nodes2))
    }
  }
  out
}

converge_to_igraph <- function(object){
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