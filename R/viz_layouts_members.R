#' Layout algorithms based on membership functions
#' @name layouts
#' @inheritParams transform
#' @param radius A vector of radii at which the concentric circles
#'   should be located.
#'   By default this is equal placement around an empty centre, 
#'   unless one (the core) is a single node,
#'   in which case this node occupies the centre of the graph.
#' @param order.by An attribute label indicating the (decreasing) order
#'   for the nodes around the circles. 
#'   By default ordering is given by a bipartite placement that reduces
#'   the number of edge crossings.
#' @family mapping
#' @source
#' Diego Diez, Andrew P. Hutchins and Diego Miranda-Saavedra. 2014.
#' "Systematic identification of transcriptional regulatory modules from
#' protein-protein interaction networks". 
#' _Nucleic Acids Research_, 42 (1) e6.
#' @examples 
#' autographr(mpn_elite_mex, layout = "concentric")
#' autographr(mpn_elite_usa_advice, layout = "concentric")
#' @export
layout_tbl_graph_concentric <- function(object, membership = NULL, radius = NULL, 
                                        order.by = NULL, 
                                        circular = FALSE, times = 1000){
  if (is.null(membership)){
    if(!is_twomode(object)) 
      membership <- to_list(node_core(object))
    else membership <- to_list(node_mode(object))
  }
  all_c  <- unlist(membership, use.names = FALSE)
  if (any(table(all_c) > 1)) 
    stop("Duplicated nodes in layers!")
  if(is_labelled(object))
    all_n <- node_names(object) else
      all_n <- 1:graph_nodes(object)
  sel_other  <- all_n[!all_n %in% all_c]
  if (length(sel_other) > 0) 
    membership[[length(membership) + 1]] <- sel_other
  if (is.null(radius)) {
    radius <- seq(0, 1, 1/(length(membership)))
    if (length(membership[[1]]) == 1) 
      radius <- radius[-length(radius)] else 
        radius <- radius[-1]
  }
  if (!is.null(order.by)){
    order.values <- lapply(order.by, 
                           function(b) node_attribute(object, b))
  } else {
    for(k in 2:length(membership)){
      xnet <- as_matrix(to_multilevel(object))[membership[[k-1]], 
                                               membership[[k]]]
      lo <- igraph::layout.bipartite(as_igraph(xnet, twomode = TRUE))
      lo <- as.data.frame(lo)
      lo$names <- node_names(object)
      if(ncol(lo)==2) lo[,1] <- 1:nrow(lo)
      order.values <- lapply(1:0, function(x)
        if(ncol(lo)>=3) sort(lo[lo[,2]==x,])[,3] 
        else sort(lo[lo[,2]==x,1]) ) 
    }
    # order.values <- getNNvec(object, members)
  }
  res <- matrix(NA, nrow = length(all_n), ncol = 2)
  for (k in 1:length(membership)) {
    r <- radius[k]
    l <- order.values[[k]]
    if(is_labelled(object))
      l <- match(l, node_names(object))
    coords <- getCoordinates(l, r)
    res[l, ] <- coords
  }
  res <- as.data.frame(res)
  names(res) <- c("x","y")
  res
}

to_list <- function(members){
  out <- lapply(sort(unique(members)), function(x){
    y <- which(members==x)
    if(!is.null(names(y))) names(y) else y
  })
  names(out) <- unique(members)
  out
}

getNNvec <- function(object, members){
  lapply(members, function(circle){
    diss <- 1 - cor(to_multilevel(as_matrix(object))[, circle])
    diag(diss) <- NA
    if(is_labelled(object))
      starts <- names(sort(node_degree(object)[circle], decreasing = TRUE)[1])
    else starts <- paste0("V",1:graph_nodes(object))[sort(node_degree(object)[circle], decreasing = TRUE)[1]]
    if(length(circle)>1)
      starts <- c(starts, names(which.min(diss[starts,])))
    out <- starts
    if(length(circle)>2){
      for(i in 1:(length(circle)-2)){
        diss <- diss[,!colnames(diss) %in% starts]
        if(is.matrix(diss)){
          side <- names(which.min(apply(diss[starts,], 1, min, na.rm = TRUE)))
          new <- names(which.min(diss[side,]))
        } else {
          side <- names(which.min(diss[starts]))
          new <- setdiff(circle,out)
        }
        if(side == out[1]){
          out <- c(new, out)
          starts <- c(new, starts[2])
        } else {
          out <- c(out, new)
          starts <- c(starts[1], new)
        }
      }
    }
    out
  })
}

getCoordinates <- function(x, r){
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

# layout_tbl_graph_railway
