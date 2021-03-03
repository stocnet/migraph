#' Blockmodelling
#' @param object A migraph-consistent object (matrix, igraph, tidygraph). 
# #' Passing the function a list apply the algorithm to the joined matrices.
#' @param p An integer representing the desired number of partitions.
#' @param cutoff A value between 0 and 1 used to determine convergence.
#' @param max.iter An integer representing the maximum number of iterations.
#' @param block.content A string indicating which method to use for
#' calculating block content.
#' Options are: "density", "sum", "meanrowsum", "meancolsum",
#' "median", "min", "max".
#' @name blockmodel
#' @source \url{https://github.com/aslez/concoR}
#' @references Breiger, R.L., Boorman, S.A., and Arabie, P.  1975.  
#' An Algorithm for Clustering Relational Data with Applications to 
#' Social Network Analysis and Comparison with Multidimensional Scaling. 
#' \emph{Journal of Mathematical Psychology}, 12: 328--383.
#' @importFrom stats cor median
#' @examples 
#' mex_concor <- blockmodel_concor(mpn_elite_mex)
#' mex_concor
#' plot(mex_concor)
#' usa_concor <- blockmodel_concor(mpn_elite_usa_advice)
#' usa_concor
#' plot(usa_concor)
#' @export
blockmodel_concor <- function(object, p = 1, 
                           cutoff = 0.999, max.iter = 25, 
                           block.content = "density"){
  
  if(is.list(object) & !is.tbl_graph(object)){
    mat <- lapply(object, function(x) as_matrix(x))
    mat <- do.call(rbind, mat)
  } else mat <- as_matrix(object)
  
  concor <- function(mat, p, cutoff, max.iter){
    partition <- function(m0, cutoff, max.iter) {
      if (ncol(m0) < 2) stop("Too few columns to partition.")
      mi <- stats::cor(m0)
      iter <- 1
      while(any(abs(mi) <= cutoff) & iter <= max.iter) {
        mi <- cor(mi)
        iter <- iter + 1
      }
      group <- mi[, 1] > 0
      list(m0[, group, drop = FALSE], m0[, !group, drop = FALSE])
    }
    p_list <- list(mat)
    for(i in 1:p) {
      p_list <- unlist(lapply(p_list, 
                              function(x) partition(x, cutoff, max.iter)), 
                       recursive = FALSE)
    }
    block_names <- function(p_list) {
      lapply(seq_along(p_list), 
             function(x) data.frame(block = x, 
                                    vertex = colnames(p_list[[x]]),
                                    stringsAsFactors = FALSE))
    }
    # blockmodelling
    df <- do.call(rbind, block_names(p_list))
    df[match(colnames(mat), df$vertex), ]
  }  
    
  if(is_twomode(mat)){
    memb <- list()
    memb$nodes1 <- concor(t(mat), p, cutoff, max.iter)
    memb$nodes2 <- concor(mat, p, cutoff, max.iter)
  } else {
    memb <- vector()
    memb <- concor(mat, p, cutoff, max.iter)
  }
  
  if(length(dim(mat)) > 2){
    d <- mat
  } else {
    d <- array(dim = c(1, nrow(mat), ncol(mat)))
    d[1, , ] <- mat
  }
  
  if(is_twomode(mat)){
    b1 <- memb$nodes1$block
    rn <- max(b1)
    b2 <- memb$nodes2$block
    cn <- max(b2)
  } else {
    b1 <- b2 <- memb$block
    rn <- cn <- max(b1)
  }
  rm <- dim(d)[1]
  bm <- array(dim = c(rm, rn, cn))
  for (i in 1:rm) for (j in 1:rn) for (k in 1:cn) {
    if (block.content == "density") 
      bm[i, j, k] <- mean(d[i, b1 == j, b2 == k, drop = FALSE], 
                          na.rm = TRUE)
    else if (block.content == "meanrowsum") {
      bm[i, j, k] <- mean(apply(d[i, b1 == j, b2 == k, drop = FALSE], 
                                2, sum, na.rm = TRUE))
    }
    else if (block.content == "meancolsum") {
      bm[i, j, k] <- mean(apply(d[i, b1 == j, b2 == k, drop = FALSE], 
                                3, sum, na.rm = TRUE))
    }
    else if (block.content == "sum") {
      bm[i, j, k] <- sum(d[i, b1 == j, b2 == k, drop = FALSE], 
                         na.rm = TRUE)
    }
    else if (block.content == "median") {
      bm[i, j, k] <- stats::median(d[i, b1 == j, b2 == k, drop = FALSE], 
                            na.rm = TRUE)
    }
    else if (block.content == "min") {
      bm[i, j, k] <- min(d[i, b1 == j, b2 == k, drop = FALSE], 
                         na.rm = TRUE)
    }
    else if (block.content == "max") {
      bm[i, j, k] <- max(d[i, b1 == j, b2 == k, drop = FALSE], 
                         na.rm = TRUE)
    }
    else if (block.content == "types") {
      temp <- mean(d[i, b1 == j, b2 == k, drop = FALSE], 
                   na.rm = TRUE)
      if (is.nan(temp)) 
        bm[i, j, k] <- "NA"
      else if (temp == 0) 
        bm[i, j, k] <- "null"
      else if (temp == 1) 
        bm[i, j, k] <- "complete"
      else if (all(apply(d[i, b1 == j, b2 == k, drop = FALSE], 
                         2, sum, na.rm = TRUE) > 0, apply(d[i, b1 == j, 
                                                            b2 == k, drop = FALSE], 3, sum, na.rm = TRUE) > 
                   0)) 
        bm[i, j, k] <- "1 covered"
      else if (all(apply(d[i, b1 == j, b2 == k, drop = FALSE], 
                         2, sum, na.rm = TRUE) > 0)) 
        bm[i, j, k] <- "1 row-covered"
      else if (all(apply(d[i, b1 == j, b2 == k, drop = FALSE], 
                         3, sum, na.rm = TRUE) > 0)) 
        bm[i, j, k] <- "1 col-covered"
      else bm[i, j, k] <- "other"
    }
  }
  
  out <- list()
  out$blocked.data <- mat
  out$plabels <- dimnames(mat)
  if(is_twomode(mat)){
    out$membership <- list(nodes1 = memb$nodes1$block, nodes2 = memb$nodes2$block)
    out$block.membership <- list(nodes1 = sort(memb$nodes1$block), nodes2 = sort(memb$nodes2$block))
    out$order.vector <- list(nodes1 = unlist(lapply(1:rn, function(x) which(memb$nodes1$block==x))),
                             nodes2 = unlist(lapply(1:rn, function(x) which(memb$nodes2$block==x))))
    out$modes <- 2
  } else {
    out$membership <- memb$block
    out$block.membership <- sort(memb$block)
    out$order.vector <- unlist(lapply(1:rn, function(x) which(memb$block==x)))
    out$modes <- 1
  }
  out$cluster.method <- "CONCOR"
  out$block.content <- block.content
  if (dim(bm)[1] == 1) {
    out$block.model <- bm[1, , ]
    # rownames(out$block.model) <- rlabels
    # colnames(out$block.model) <- rlabels
  } else {
    out$block.model <- bm
    # dimnames(out$block.model) <- list(glabels, rlabels, rlabels)
  }
  attr(out, "class") <- "blockmodel"
  out
    
}

#' @rdname blockmodel
#' @param x An object of class "blockmodel"
#' @param ... Additional arguments passed to generic print method
#' @export
print.blockmodel <- function (x, ...){
  cat("\nNetwork Blockmodel:\n\n")
  cat("Block membership:\n\n")
  if(x$modes == 1){
    if (is.null(x$plabels)){
      plab <- seq_len(x$block.membership)[x$order.vector]
    } else {
      plab <- x$plabels[[1]]
    } 
    temp <- matrix(x$block.membership, nrow = 1)
    dimnames(temp) <- list("", plab)
    print(temp[1, order(x$order.vector)])
  } else {
    cat("  First nodeset:\n\n")
    if (is.null(x$plabels)){
      plab <- seq_len(x$block.membership$nodes1)[x$order.vector$nodes1]
    } else {
      plab <- x$plabels[[1]]
    } 
    temp <- matrix(x$block.membership$nodes1, nrow = 1)
    dimnames(temp) <- list("", plab)
    print(temp[1, order(x$order.vector$nodes1)], ...)
    cat("\n  Second nodeset:\n\n")
    if (is.null(x$plabels)){
      plab <- seq_len(x$block.membership$nodes2)[x$order.vector$nodes2]
    } else {
      plab <- x$plabels[[2]]
    } 
    temp <- matrix(x$block.membership$nodes2, nrow = 1)
    dimnames(temp) <- list("", plab)
    print(temp[1, order(x$order.vector$nodes2)], ...)
  }
  cat("\nReduced form blockmodel:\n\n")
  if (length(dim(x$block.model)) > 2) {
    for (i in 1:dim(x$block.model)[1]) {
      temp <- x$block.model[i, , ]
      dimnames(temp) <- list(x$rlabels, x$rlabels)
      cat("\t", x$glabels[i], "\n")
      print(temp, ...)
      cat("\n")
    }
  }
  else {
    temp <- x$block.model
    dimnames(temp) <- list(x$rlabels, x$rlabels)
    cat("\t", x$glabels, "\n")
    print(temp, ...)
  }
}

#' @rdname blockmodel
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot geom_tile aes scale_fill_gradient theme_grey labs theme scale_x_discrete scale_y_discrete geom_vline geom_hline element_blank element_text
#' @importFrom rlang .data
#' @export
plot.blockmodel <- function(x, ...){

  plot_data <- x$blocked.data
  plot_data <- as.data.frame(plot_data) %>%
    tibble::rownames_to_column("Var1") %>%
    tidyr::pivot_longer(!.data$Var1, names_to = "Var2", values_to = "value")
  
  g <- ggplot2::ggplot(plot_data, ggplot2::aes(.data$Var2, .data$Var1)) + 
    ggplot2::geom_tile(ggplot2::aes(fill = .data$value), colour = "white") + 
    ggplot2::scale_fill_gradient(low = "white", high = "black") +
    ggplot2::theme_grey(base_size = 9) + 
    ggplot2::labs(x = "", y = "") + 
    ggplot2::theme(legend.position = "none",
                   axis.ticks = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(size = 9 * 0.8, colour = "grey50"),
                   axis.text.x = ggplot2::element_text(size = 9 * 0.8, 
                                                       angle = 30, hjust = 0, colour = "grey50"))
  
  if(x$modes==1){
    g <- g + ggplot2::scale_x_discrete(expand = c(0, 0), position = "top", limits = colnames(x$blocked.data)[x$order.vector]) +
      ggplot2::scale_y_discrete(expand = c(0, 0), limits = rev(rownames(x$blocked.data)[x$order.vector])) + 
      ggplot2::geom_vline(xintercept = c(1+which(diff(x$block.membership)!=0))-.5, colour = "red") +
      ggplot2::geom_hline(yintercept = nrow(x$blocked.data) - c(1+which(diff(x$block.membership)!=0))+1.5, colour = "red")
  } else {
    g <- g + ggplot2::scale_y_discrete(expand = c(0, 0), limits = rev(rownames(x$blocked.data)[x$order.vector$nodes1])) +
      ggplot2::scale_x_discrete(expand = c(0, 0), position = "top", limits = colnames(x$blocked.data)[x$order.vector$nodes2]) + 
      ggplot2::geom_vline(xintercept = c(1+which(diff(x$block.membership$nodes2)!=0))-.5, colour = "blue") +
      ggplot2::geom_hline(yintercept = nrow(x$blocked.data) - c(1+which(diff(x$block.membership$nodes1)!=0))+1.5, colour = "red")
  }
  g
  
}
