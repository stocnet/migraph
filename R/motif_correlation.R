#' Node correlation
#' 
#' @description 
#'   This function performs a Pearson pairwise correlation on a given matrix or network data.
#'   It includes a switch: 
#'   whereas for a two-mode network it will perform a regular correlation,
#'   including all rows,
#'   for an undirected network it will perform a correlation on a matrix 
#'   with the diagonals removed,
#'   for a reciprocated network it will include the difference
#'   between reciprocated ties,
#'   and for complex networks it will include also the difference 
#'   between the self ties in each pairwise calculation.
#'   This function runs in \eqn{O(mn^2)} complexity.
#' @name node_correlation
#' @inheritParams cohesion
#' @family motifs
#' @export
to_correlation <- function(.data){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  mat <- manynet::as_matrix(.data)
  if(manynet::is_twomode(.data)){
    # if(!any(colnames(m0) %in% rownames(m0))) 
    #   mat <- node_tie_census(mat)
    out <- .corTwomode(mat)
  } else if(manynet::is_complex(.data)){
    out <- .corComplex(mat)
  } else if(manynet::is_directed(.data)){
    out <- .corRecip(mat)
  } else {
    out <- .corDiag(mat)
  }
  out
}
  
.corTwomode <- function(m0){
  stats::cor(m0)
}

# Though warnings need to be suppressed,
# this is bench::mark()ed at about 17-18 times faster than corrColsExcludeDiag(),
# and uses 188 times less memory
.corDiag <- function(M){
  diag(M) <- NA
  out <- suppressWarnings(stats::cor(M, use = "pairwise.complete.obs"))
  out[is.na(out)] <- 0
  diag(out) <- 1
  out
}

# Though warnings need to be suppressed,
# this is bench::mark()ed at about 2 times faster than corrColsRecipRLB()
.corRecip <- function(M){
  all.pairs <- combn(1:ncol(M),2)
  corres <- apply(all.pairs, 2, function(i){
    x <- c(M[-i,i[1]], M[i[1],i[2]])
    y <- c(M[-i,i[2]], M[i[2],i[1]])
    suppressWarnings(stats::cor(x = x, y = y))
  })
  out <- matrix(1,nrow(M),ncol(M))
  out[lower.tri(out)] <- corres
  out <- .makeSymm(out)
  out[is.na(out)] <- 0
  diag(out) <- 1
  rownames(out) <- rownames(M)
  colnames(out) <- colnames(M)
  out
}

# Though warnings need to be suppressed,
# this is bench::mark()ed at about 2.3 times faster than corrColsRecipUCI()
.corComplex <- function(M){
  all.pairs <- combn(1:ncol(M),2)
  corres <- apply(all.pairs, 2, function(i){
    x <- c(M[-i,i[1]], M[i[1],i[2]], M[i[1],i[1]])
    y <- c(M[-i,i[2]], M[i[2],i[1]], M[i[2],i[2]])
    suppressWarnings(stats::cor(x = x, y = y))
  })
  out <- matrix(1,nrow(M),ncol(M))
  out[lower.tri(out)] <- corres
  out <- .makeSymm(out)
  out[is.na(out)] <- 0
  diag(out) <- 1
  rownames(out) <- rownames(M)
  colnames(out) <- colnames(M)
  out
}

.makeSymm <- function(m) {
  m[upper.tri(m)] <- t(m)[upper.tri(m)]
  return(m)
}
