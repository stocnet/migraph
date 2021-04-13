#' Linear regression for multimodal network data
#' 
#' This function extends the multiple regression quadratic assignment procedure (MRQAP)
#' of network linear model to two mode networks. 
#' @name netlm
#' @param formula A formula describing the relationship being tested.
#' @param data A named list of matrices, graphs, or a tidygraph object.
#' @param ... Arguments passed on to `lm()`.
#' @importFrom dplyr bind_cols
#' @importFrom purrr map
#' @importFrom stats lm
#' @examples
#' mat1 <- matrix(c(0,1,1,0,0,1,1,1),4,2)
#' mat2 <- matrix(c(0,1,0,1,0,1,0,1),4,2)
#' mat3 <- matrix(c(0,0,1,1,0,0,1,1),4,2)
#' lmat <- list(mat1 = mat1, mat2 = mat2, mat3 = mat3)
#' model1 <- netlm(mat1 ~ mat2 + mat3, lmat)
#' summary(model1)
#' @export
netlm <- function(formula, data, ...){
  
  if(!is.list(data)) stop("netlm() expects a list of matrices.")
  if(!is.matrix(data[[1]])) stop("netlm() expects a list of matrices.")
  
  orig <- data
  data <- dplyr::bind_cols(purrr::map(data, function(x) c(x)))
  out <- lm(formula, data, ...)
  class(out) <- "netlm"
  out$call <- match.call()
  out$matrices <- orig
  invisible(out)
}

#' @rdname netlm
#' @param object an object of class "netlm", usually as a result of a call to `netlm()`.
#' @param reps Integer indicating the number of draws to use for quantile estimation. 
#' (Relevant to the null hypothesis test only - the analysis itself is unaffected by this parameter.) 
#' Note that, as for all Monte Carlo procedures, convergence is slower for more extreme quantiles. 
#' By default, reps=1000.
#' @param ... Arguments passed on to `lm()`.
#' @importFrom stats ecdf lm
#' @export
summary.netlm <- function(object, reps = 1000, ...){
  
  if(class(object)!="netlm") stop("This function expects an object of class 'netlm'.")
  
  xn <- attr(object$terms, "term.labels")
  yn <- as.character(object$terms[[2]])

  # Selecting the matrices in the data list
  IV <- object$matrices[xn]
  DV <- object$matrices[yn][[1]]
  
  # Permutation, list of matrices
  rbperm <- function (m) {
    n <- sample(1:dim(m)[1])
    o <- sample(1:dim(m)[2])
    p <- matrix(data = m[n, o], nrow = dim(m)[1], ncol = dim(m)[2])
    p
  }
  
  permDist <- matrix(0, reps, length(object$coefficients))
  for(i in 1:reps){
    tempDV <- rbperm(DV)
    permDist[i,] <- (lm(as.numeric(unlist(tempDV)) ~
                          Reduce(cbind,lapply(1:length(IV),
                                              function(x) unlist(IV[x][1]))), ...))$coefficients
  }
  
  out <- object
  class(out) <- "summary.netlm"
  out$permDist <- permDist
  coefs <- object$coefficients
  out$pvals <- as.numeric(lapply(1:length(coefs),
                             function(p) ecdf(out$permDist[,p])(coefs[p])))
  

  # Calculate R-Squared
  r <- object$residuals
  qr.lm <- function (x, ...) {
    if (is.null(r <- x$qr)) 
      stop("lm object does not have a proper 'qr' component.\n Rank zero or should not have used lm(.., qr=FALSE).")
    r
  }
  Qr <- qr.lm(object)
  n <- NROW(Qr$qr)
  f <- object$fitted.values
  rdf <- object$df.residual
  mss <- if (attr(object$terms, "intercept")) 
    sum((f - mean(f))^2)
  else sum(f^2)
  rss <- sum(r^2)
  df.int <- if (attr(object$terms, "intercept")) 
    1L
  else 0L
  out$r.squared <- mss/(mss + rss)
  out$adj.r.squared <- 1 - (1 - out$r.squared) * ((n - 
                                                     df.int)/rdf)
  
  out
}

#' @rdname netlm
#' @param x an object of class "summary.netlm", usually, a result of a call to `summary.netlm()`.
#' @param digits the number of significant digits to use when printing.
#' @param signif.stars logical. If TRUE, ‘significance stars’ are printed for each coefficient.
#' @importFrom stats printCoefmat
#' @export
print.summary.netlm <- function(x,
                                digits = max(3, getOption("digits") - 3),
                                signif.stars = getOption("show.signif.stars"),
                                ...){
  
  if(class(x)!="summary.netlm") stop("This function expects an object of class 'summary.netlm'.")
  
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), 
      "\n\n", sep = "")
  
  cat("\nCoefficients:\n")
  coefs <- x$coefficients
  pvals <- x$pvals
  coefs <- cbind(coefs, pvals)
  printCoefmat(coefs, 
               digits = digits, signif.stars = signif.stars, 
               P.values = TRUE, has.Pvalue = TRUE,
               na.print = "NA")
  cat("\n")
  
  cat("Multiple R-squared: ", formatC(x$r.squared, digits = digits))
  cat(",\tAdjusted R-squared: ", formatC(x$adj.r.squared, digits = digits))
  cat("\n")
  
}
