make_node_motif <- function(out, object){
  class(out) <- c("node_motif", class(out))
  attr(out, "mode") <- node_mode(object)
  out
}

make_graph_motif <- function(out, object){
  class(out) <- c("graph_motif", class(out))
  attr(out, "mode") <- graph_dims(object)
  out
}

#' @export
print.node_motif <- function(x, ...,
                         max.length = 6,
                         digits = 3){
  if(any(attr(x, "mode"))){
    print(dplyr::tibble(as.data.frame(x)[!attr(x, "mode")]))
    print(dplyr::tibble(as.data.frame(x)[attr(x, "mode")]))
  } else {
    print(dplyr::tibble(as.data.frame(x)))
  }
}

# summary(node_triad_census(mpn_elite_mex), 
#           node_regular_equivalence(mpn_elite_mex, select = "elbow"))
#' @export
summary.node_motif <- function(object, ...,
                                membership,
                                FUN = mean){
  out <- t(sapply(unique(membership), 
                  function(x) if(sum(membership==x)==1) object[membership==x,] else
                    apply(object[membership == x, ], 2, FUN)))
  rownames(out) <- paste("Block", unique(membership))
  dplyr::tibble(as.data.frame(out))
}

# print.node_motif <- function(x, ...,
#                                max.length = 6,
#                                digits = 3){
#   if(any(attr(x, "mode"))){
#     y <- x[attr(x, "mode")]
#     y <- y[max(1, ((length(y)-max.length)+1)):length(y)]
#     z <- x[!attr(x, "mode")]
#     z <- z[1:min(length(z), max.length)]
#     class(z) <- "numeric"
#     z <- format(z, digits = digits)
#     class(y) <- "numeric"
#     y <- format(y, digits = digits)
#     print(noquote(format(c(z,
#                            paste("+", length(x) - (length(z) + length(y)),
#                                  "others"), y))))
#   } else {
#     z <- x[1:min(length(x), max.length)]
#     class(z) <- "numeric"
#     z <- format(z, digits = digits)
#     print(noquote(format(c(z,
#                            paste("+", length(x) - length(z),
#                                  "others")))))
#   }
# }
# 

