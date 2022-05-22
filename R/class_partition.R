make_partition <- function(out, object){
  class(out) <- c("partition", class(out))
  attr(out, "mode") <- node_mode(object)
  out
}

#' @export
print.partition <- function(x, ...,
                               max.length = 6,
                               digits = 3){
  if(any(attr(x, "mode"))){
    for(i in names(table(x))){
      if(i == names(table(x))[1]) cat(i, "\n")
      else cat("\n", i, "\n")
      if(!is.null(names(x))){
        y <- paste(names(x[x==i & attr(x, "mode")]), collapse = ", ")
        z <- paste(names(x[x==i & !attr(x, "mode")]), collapse = ", ")
      } else{
        y <- paste(which(x==i & attr(x, "mode")), collapse = ", ")
        z <- paste(which(x==i & !attr(x, "mode")), collapse = ", ")
      } 
      cat("  ", y, "\n")
      cat("  ", z)
    }
  } else {
    for(i in names(table(x))){
      if(i == names(table(x))[1]) cat(i, "\n")
      else cat("\n", i, "\n")
      if(!is.null(names(x))) 
        y <- paste(names(x[x==i]), collapse = ", ")
      else 
        y <- paste(which(x==i), collapse = ", ")
      cat("  ", y)
    }
  }
}
