#' @title arrange.vars
#' @description Arranges data frame variables by position
#' @param data must be a data frame
#' @param vars must be a named vector, e.g. c("var.name"=1)
#' @return OUTPUT_DESCRIPTION
#' @details Credit goes to the user landroni on StackOverflow for this very useful function
#' @examples arrange.vars(data, c("Out"=2, "Files"=1, "Time"=4))
#' @source https://stackoverflow.com/questions/5620885/how-does-one-reorder-columns-in-a-data-frame
#' @export 
arrange.vars <- function(data, vars){
  ##stop if not a data.frame (but should work for matrices as well)
  stopifnot(is.data.frame(data))
  
  ##sort out inputs
  data.nms <- names(data)
  var.nr <- length(data.nms)
  var.nms <- names(vars)
  var.pos <- vars
  ##sanity checks
  stopifnot( !any(duplicated(var.nms)), 
             !any(duplicated(var.pos)) )
  stopifnot( is.character(var.nms), 
             is.numeric(var.pos) )
  stopifnot( all(var.nms %in% data.nms) )
  stopifnot( all(var.pos > 0), 
             all(var.pos <= var.nr) )
  
  ##prepare output
  out.vec <- character(var.nr)
  out.vec[var.pos] <- var.nms
  out.vec[-var.pos] <- data.nms[ !(data.nms %in% var.nms) ]
  stopifnot( length(out.vec)==var.nr )
  
  ##re-arrange vars by position
  data <- data[ , out.vec]
  return(data)
}