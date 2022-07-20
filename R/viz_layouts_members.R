#' Layout algorithms based on membership functions
#' @name layouts
#' @family mapping
#' @source
#' Diego Diez, Andrew P. Hutchins and Diego Miranda-Saavedra. 2014.
#' "Systematic identification of transcriptional regulatory modules from
#' protein-protein interaction networks". 
#' _Nucleic Acids Research_, 42 (1) e6.
#' @examples 
#' autographr(mpn_elite_mex, layout = "concentric")
#' @export
layout_tbl_graph_concentric <- function(object, members = NULL, radius = NULL, 
                                        order.by = NULL, 
                                        circular = FALSE, times = 1000){
  if (is.null(members)){
    if(!is_twomode(object)) members <- to_list(node_core(object))
    else members <- to_list(node_mode(object))
  }
  all_c  <- unlist(members, use.names = FALSE)
  if (any(table(all_c) > 1)) 
    stop("Duplicated nodes in layers!")
  all_n = node_names(object)
  sel_other = all_n[!all_n %in% all_c]
  if (length(sel_other) > 0) 
    members[[length(members) + 1]] <- sel_other
  if (is.null(radius)) {
    radius <- seq(0, 1, 1/(length(members)))
    if (length(members[[1]]) == 1) 
      radius <- radius[-length(radius)]
    else radius <- radius[-1]
  }
  if (!is.null(order.by)) 
    order.values <- lapply(order.by, 
                           function(b) get.vertex.attribute(g, b))
  res <- matrix(NA, nrow = length(all_n), ncol = 2)
  for (k in 1:length(members)) {
    r <- radius[k]
    l <- members[[k]]
    i <- which(node_names(object) %in% l) - 1
    i_o <- i
    if (!is.null(order.by)) {
      ob <- lapply(order.values, function(v) v[i + 1])
      ord <- do.call(order, ob)
      i_o <- i_o[ord]
    }
    res[i_o + 1, ] <- .getCoordinates(i_o, r)
  }
  res <- as.data.frame(res)
  names(res) <- c("x","y")
  res
}

.getCoordinates <- function(x, r){
  l = length(x)
  d = 360/l
  c1 = seq(0, 360, d)
  c1 = c1[1:(length(c1) - 1)]
  tmp = t(sapply(c1, 
                 function(cc) c(cos(cc * pi/180) * 
                                  r, sin(cc *
                                           pi/180) * r)))
  rownames(tmp) = x
  tmp
}

to_list <- function(members){
  out <- lapply(sort(unique(members)), function(x){
    y <- which(members==x)
    if(!is.null(names(y))) names(y) else y
  })
  names(out) <- unique(members)
  out
}

