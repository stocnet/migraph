#' Linear regression for multimodal network data
#' 
#' This function extends the multiple regression quadratic assignment procedure (MRQAP)
#' of network linear model to two mode networks. 
#' @name netlm
#' @param formula A formula describing the relationship being tested.
#' @param data A named list of matrices, graphs, or a tidygraph object.
#' @param ... Arguments passed on to `lm()`.
#' @param reps Integer indicating the number of draws to use for quantile estimation. 
#' (Relevant to the null hypothesis test only - the analysis itself is unaffected by this parameter.) 
#' Note that, as for all Monte Carlo procedures, convergence is slower for more extreme quantiles. 
#' By default, reps=1000.
#' @importFrom dplyr bind_cols
#' @importFrom purrr map
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
#' @param rep number of permutations to conduct.
#' @export
summary.netlm <- function(object, rep = 1000, ...){
  
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
  
  permDist <- matrix(0, rep, length(object$coefficients))
  for(i in 1:rep){
    tempDV <- rbperm(DV)
    permDist[i,] <- (lm(as.numeric(unlist(tempDV)) ~
                          Reduce(cbind,lapply(1:length(IV),
                                              function(x) unlist(IV[x][1])))))$coefficients
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


#############################################################
#############################################################


# netlm2 <- function(DV, IV, names, rep = 1000){
#   
#   if(missing(names)){ 
#     names <- paste0("x", 1:length(IV))
#   }
#   # Consider converting to formula
#   
#   rbperm <- function (m) {
#     n <- sample(1:dim(m)[1])
#     o <- sample(1:dim(m)[2])
#     p <- matrix(data = m[n, o], nrow = dim(m)[1], ncol = dim(m)[2])
#     p
#   }
#   
#   nIV <- length(IV)
#   M.fit <- lm(as.numeric(unlist(DV)) ~ Reduce(cbind,
#                                               lapply(1:length(IV), function(x) unlist(IV[x][1]))))
#   M.coeff <- M.fit$coefficients
#   
#   permDist <- matrix(0, rep, (nIV+1))
#   
#   for(i in 1:rep){
#     tempDV <- rbperm(DV)
#     permDist[i,] <- (lm(as.numeric(unlist(tempDV)) ~ 
#                           Reduce(cbind,lapply(1:length(IV), 
#                                               function(x) unlist(IV[x][1])))))$coefficients
#   }
#   
#   resTable <- data.frame(Effect = c("Intercept", names), 
#                          Coefficients = formatC(M.coeff, format = "f", digits = 2),
#                          Pvalue = signif(as.numeric(lapply(1:(nIV+1), 
#                                                            function(x) ecdf(permDist[,x])(M.coeff[x]))), 
#                                          digits = 2),
#                          Sig = ifelse(as.numeric(lapply(1:(nIV+1), 
#                                                         function(x) ecdf(permDist[,x])(M.coeff[x])))<0.05, 
#                                       ifelse(as.numeric(lapply(1:(nIV+1), 
#                                                                function(x) ecdf(permDist[,x])(M.coeff[x])))<0.01, 
#                                              ifelse(as.numeric(lapply(1:(nIV+1), 
#                                                                       function(x) ecdf(permDist[,x])(M.coeff[x])))<0.001, 
#                                                     "***", "**"), "*"), ""))
#   rownames(resTable) <- NULL
#   print(resTable)
#   # Turn this into a print function
#   
#   cat("\nMultiple R-squared: ", formatC(summary(M.fit)$r.squared),
#       ",\tAdjusted R-squared: ", formatC(summary(M.fit)$adj.r.squared),
#       "\n", sep="")
#   
#   obj <- list()
#   obj$results <- data.frame(Effect = c("Intercept", names), 
#                             Coefficients = as.numeric(formatC(M.coeff, format="f", digits = 2)),
#                             Pvalue = signif(as.numeric(lapply(1:(nIV+1), 
#                                                               function(x) ecdf(permDist[,x])(M.coeff[x]))), 
#                                             digits=2),
#                             Sig = ifelse(as.numeric(lapply(1:(nIV+1), 
#                                                            function(x) ecdf(permDist[,x])(M.coeff[x])))<0.05, 
#                                          ifelse(as.numeric(lapply(1:(nIV+1), 
#                                                                   function(x) ecdf(permDist[,x])(M.coeff[x])))<0.01, 
#                                                 ifelse(as.numeric(lapply(1:(nIV+1), 
#                                                                          function(x) ecdf(permDist[,x])(M.coeff[x])))<0.001, 
#                                                        "***", "**"), "*"), ""))
#   rownames(obj$results) <- NULL
#   obj$r.squared <- formatC(summary(M.fit)$r.squared)
#   obj$adj.r.squared <- formatC(summary(M.fit)$adj.r.squared)
#   invisible(obj)
# }
