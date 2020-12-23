#'Hierarchical clustering using CONCOR.
#'
#'Partitions relational data using a CONvergence of iterated CORrelations 
#'(CONCOR) algorithm.

#'@param m0 A list of \eqn{n} x \eqn{n} matrices, each of which refers to a
#'relation on a set of vertices. Only one-mode data are supported at present.
#'@param cutoff A value between 0 and 1 used to determine convergence.
#'@param max.iter An integer representing the maximum number of iterations.
#'@param p An integer representing the desired number of partitions.
#'@param p_list List of stacked matrix  
#'@return A \code{data.frame} depicting the block assignment for each vertex.
#'Rows are sorted to match the order in which vertices appear in the original
#'data. The column vector indicating block membership can be passed directly to
#' the \code{blockmodel} command in \code{sna}. 
#'
#'@references Breiger, R.L., Boorman, S.A., and Arabie, P.  1975.  An Algorithm
#'for Clustering Relational Data with Applications to Social Network Analysis
#'and Comparison with Multidimensional Scaling. \emph{Journal of Mathematical
#'Psychology}, 12: 328--383.
#'
#'@examples
#'data(bank_wiring)
#'library(sna)
#'b <- concor_hca(bank_wiring, p = 2)
#'g <- as.sociomatrix.sna(bank_wiring)
#'bm <- blockmodel(g, ec = b$block, glabels = names(bank_wiring))
#'plot(bm)
#'@source \url{https://github.com/aslez/concoR}
concor_hca <- function(m0, cutoff = 0.999, max.iter = 25, p = 1) {
  mat_stack <- do.call(rbind, m0)
  p_list <- list(mat_stack)
  for(i in 1:p) {
    p_list <- unlist(lapply(p_list, 
                            function(x) concor(x, cutoff, max.iter)), 
                     recursive = FALSE)
  }
  df <- do.call(rbind, block_names(p_list))
  df[match(rownames(m0[[1]]), df$vertex), ]
}

#'@rdname concor_hca
concor <- function(m0, cutoff = 0.999, max.iter = 50) {
  if (ncol(m0) < 2) stop("Too few columns to partition.")
  mi <- cor(m0)
  iter <- 1
  while(any(abs(mi) <= cutoff) & iter <= max.iter) {
    mi <- cor(mi)
    iter <- iter + 1
  }
  group <- mi[, 1] > 0
  list(m0[, group, drop = FALSE], m0[, !group, drop = FALSE])
}

#'@rdname concor_hca
block_names <- function(p_list) {
  lapply(seq_along(p_list), 
         function(x) data.frame(block = x, 
                                vertex = colnames(p_list[[x]]),
                                stringsAsFactors = FALSE))
}