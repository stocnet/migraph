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

#' @importFrom ggdendro ggdendrogram
#' @importFrom stats cutree
#' @export
plot.partition <- function(x, ...){
  hc <- attr(x, "hc")
  k <- attr(x, "k")
  memb <- x[hc$order]
  clust <- memb[!duplicated(memb)]
  colors <- ifelse(match(memb, clust) %% 2, 
                   "#000000", "#E20020")
  ggdendro::ggdendrogram(hc, rotate = TRUE) +
    ggplot2::geom_hline(yintercept = hc$height[length(hc$order) - k],
                        linetype = 2,
                        color = "#E20020") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(colour = "#E20020"),
                   axis.text.y = suppressWarnings(
                     ggplot2::element_text(colour = colors)))
}

