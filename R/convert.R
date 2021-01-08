#' Convert a two-mode data frame into an incidence matrix
#' 
#' This function takes a data frame,
#' either as a data frame version of a matrix
#' or as an edgelist,
#' and returns an incidence matrix.
#' @name convert 
#' @examples
#' test <- data.frame(id1 = c("A","B","B","C","C"),
#'                    id2 = c("I","G","I","G","H"))
#' as_incidence_matrix(test)
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
      # out <- as.matrix(table(df[,1], df[,2]))
      # class(out) <- c("matrix")
      # out <- as.numeric(out)
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