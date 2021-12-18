#' Linear regression for network data
#' 
#' This function extends the multiple regression quadratic assignment procedure
#' (MRQAP) of network linear model to two mode networks.
#' It also works with combined graph/network objects by constructing the
#' various dependent and independent matrices for the user.
#' Lastly, because it relies on an object that contains all this information
#' it can offer a more informative formula-based system for specifying the model.
#' @name regression
#' @param formula A formula describing the relationship being tested.
#' @param data A named list of matrices, graphs, or a tidygraph object.
#' @param ... Arguments passed on to `lm()`.
#' @importFrom dplyr bind_cols
#' @importFrom purrr map
#' @importFrom stats lm
#' @examples
#' messages <- mutate_edges(ison_eies, 
#'   generate_random(ison_eies), attr_name = "random")
#' model1 <- network_reg(weight ~ random + 
#'   same(Discipline) + same(Citations), messages)
#' summary(model1, reps = 200) # increase reps for publication
#' @export
network_reg <- function(formula, data, ...) {
  
  matrixList <- convertToMatrixList(formula, data)
  formula <- convertFormula(formula, matrixList)
  vectorList <- dplyr::bind_cols(purrr::map(matrixList, function(x) c(x)))
  out <- lm(formula, vectorList, ...)
  class(out) <- "netlm"
  out$call <- match.call()
  out$matrices <- matrixList
  invisible(out)
  
}

#' @importFrom stats as.formula
convertFormula <- function(formula, new_names){
  as.formula(paste(paste(formula[[2]],"~"),
                   paste(paste0("`", names(new_names)[-1], "`"), collapse = " + ")))
}

convertToMatrixList <- function(formula, data){
  data <- as_tidygraph(data)
  DV <- as_matrix(to_uniplex(data, 
                             edge = getDependentName(formula)))
  IVnames <- getRHSNames(formula)
  IVs <- lapply(IVnames, function(x){
    if(x[[1]] == "ego"){
      out <- matrix(node_attribute(data, x[[2]]),
                    nrow(DV), ncol(DV))
    } else if (x[[1]] == "alter"){
      out <- matrix(node_attribute(data, x[[2]]),
                    nrow(DV), ncol(DV), byrow = TRUE)
    } else if (x[[1]] == "same"){
      rows <- matrix(node_attribute(data, x[[2]]),
                     nrow(DV), ncol(DV))
      cols <- matrix(node_attribute(data, x[[2]]),
                     nrow(DV), ncol(DV), byrow = TRUE)
      out <- (rows==cols)*1
    } else {
      if (x[[1]] %in% graph_edge_attributes(data)){
        out <- as_matrix(to_uniplex(data, 
                                    edge = x[[1]]))
      }
    }
    out
  })
  out <- c(list(DV),IVs)
  names(out) <- c(formula[[2]],
                  sapply(IVnames, paste, collapse = " "))
  out
}

# inspired by the ergm package function parser
extractFormulaTerms <- function(rhs) {
  # most inner term reached
  if (is.symbol(rhs)) {
    return(rhs)
  }
  if (!is.call(rhs[[1]]) &&
      rhs[[1]] != "+" &&
      rhs[[1]] != "*") {
    return(rhs)
    # return(list(rightHandSide[[1]], rightHandSide[[2]]))
  } else {
    return(c(extractFormulaTerms(rhs[[2]]), rhs[[3]]))
  }
}

getRHSNames <- function(formula) {
  rhs <- extractFormulaTerms(formula[[3]])
  # embed single parameter models in list
  if (!is.list(rhs)) rhs <- list(rhs)
  rhsNames <- lapply(rhs, function(term) lapply(term, deparse))
}

getDependentName <- function(formula) {
  dep <- list(formula[[2]])
  depName <- unlist(lapply(dep, deparse))
}

#' @rdname regression
#' @param object an object of class "netlm", usually as a result of a call to
#' `network_reg()`.
#' @param reps Integer indicating the number of draws to use for quantile
#' estimation. (Relevant to the null hypothesis test only - the analysis
#' itself is unaffected by this parameter.) 
#' Note that, as for all Monte Carlo procedures, convergence is slower for more
#' extreme quantiles.
#' By default, reps=1000.
#' @param ... Arguments passed on to `lm()`.
#' @importFrom stats ecdf lm
#' @export
summary.netlm <- function(object, reps = 1000, ...) {

  # Getting necessary elements
  xn <- gsub("`", "", attr(object$terms, "term.labels"))
  yn <- as.character(object$terms[[2]])
  IV <- object$matrices[xn]
  DV <- object$matrices[yn][[1]]
  
  # Permutation, list of matrices
  permDist <- t(sapply(1:reps, function(i){
    tempDV <- as_matrix(generate_permutation(DV))
    unname((lm(as.numeric(unlist(tempDV)) ~
                           Reduce(cbind, lapply(seq_len(length(IV)),
                                                function(x) unlist(IV[x][1])))))$coefficients)
    
  }))
  
  out <- object
  class(out) <- "summary.netlm"
  out$permDist <- permDist
  coefs <- object$coefficients
  out$pvals <- as.numeric(lapply(seq_len(length(coefs)),
                             function(p) ecdf(out$permDist[, p])(coefs[p])))

  # Calculate R-Squared
  r <- object$residuals
  qr.lm <- function(x, ...) {
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
  out$r.squared <- mss / (mss + rss)
  out$adj.r.squared <- 1 - (1 - out$r.squared) * ((n - 
                                                     df.int) / rdf)
  out
}

#' @rdname regression
#' @param x an object of class "summary.netlm", usually, a result of a call to
#' `summary.netlm()`.
#' @param digits the number of significant digits to use when printing.
#' @param signif.stars logical. If TRUE, ‘significance stars’ are printed for
#' each coefficient.
#' @importFrom stats printCoefmat
#' @export
print.summary.netlm <- function(x,
                                digits = max(3, getOption("digits") - 3),
                                signif.stars = getOption("show.signif.stars"),
                                ...) {
  
  if (class(x) != "summary.netlm") {
    stop("This function expects an object of class 'summary.netlm'.")
  }
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"),
      "\n\n", sep = "")
  cat("\nCoefficients:\n")
  coefs <- x$coefficients
  names(coefs) <- gsub("`", "", names(coefs))
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

