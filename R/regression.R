#' Linear regression for network data
#' 
#' This function provides a parallelised implementation of
#' the multiple regression quadratic assignment procedure (MRQAP)
#' for both one-mode and two-mode network linear models.
#' It offers several advantages:
#' - it works with combined graph/network objects such as igraph and network objects
#' by constructing the various dependent and independent matrices for the user.
#' - it offers a more intuitive formula-based system for specifying the model.
#' - it offers more straightforward printing of results that can combine with
#' results from fitting other models.
#' @name regression
#' @param formula A formula describing the relationship being tested.
#' @param data An igraph, network, or tidygraph object.
#' @param method A method for establishing the null hypothesis.
#' Currently 
#' Note that "qap" currently defaults to Dekker et al's double semi-partialling technique.
#' @param times Integer indicating the number of draws to use for quantile
#' estimation. (Relevant to the null hypothesis test only - the analysis
#' itself is unaffected by this parameter.) 
#' Note that, as for all Monte Carlo procedures, convergence is slower for more
#' extreme quantiles.
#' By default, times=1000.
#' 1,000 - 10,000 repetitions recommended for publication-ready results.
#' @param ... Arguments passed on to `lm()`.
#' @importFrom dplyr bind_cols
#' @importFrom purrr map
#' @importFrom stats lm
#' @examples
#' messages <- mutate_edges(ison_eies, 
#'   generate_random(ison_eies), attr_name = "random")
#' tictoc::tic()
#' (model1 <- network_reg(weight ~ random + 
#'   same(Discipline) + same(Citations), messages, times = 500))
#' tictoc::toc()
#' library(profvis)
#' profvis(model1 <- network_reg(weight ~ random + 
#'   same(Discipline) + same(Citations), messages, times = 500))
#' tictoc::tic()
#' (model2 <- network_reg(weight ~ random + 
#'   same(Discipline) + same(Citations), data = messages, 
#'   method = "netlm", times = 500))
#' tictoc::toc()
#' library(microbenchmark)
#' microbenchmark(network_reg(weight ~ random + 
#'   same(Discipline) + same(Citations), messages, times = 100),
#'   network_reg(weight ~ random + 
#'   same(Discipline) + same(Citations), messages, times = 100, method = "netlm"),
#'   times = 25)
#' tidy(model1)
#' @export
network_reg <- function(formula, data,
                        method = c("qap","netlm"),
                        times = 1000, 
                        verbose = FALSE,
                        ...) {
  
  matrixList <- convertToMatrixList(formula, data)
  convForm <- convertFormula(formula, matrixList)
  
  method <- match.arg(method)
    g <- matrixList
    intercept <- FALSE
    nx <- length(matrixList) - 1
    n <- dim(matrixList[[1]])
    
    directed <- ifelse(is_directed(matrixList[[1]]), "digraph", "graph")
    diag <- is_complex(matrixList[[1]])

    if (any(sapply(lapply(g, is.na), any))) 
      stop("Missing data supplied; this may pose problems for certain null hypotheses.")
    
    fit.base <- gfit(g, 
                     directed = directed, 
                     diag = diag, 
                     rety = TRUE)
    fit <- list()
    fit$coefficients <- qr.coef(fit.base[[1]], fit.base[[2]])
    fit$fitted.values <- qr.fitted(fit.base[[1]], fit.base[[2]])
    fit$residuals <- qr.resid(fit.base[[1]], fit.base[[2]])
    fit$qr <- fit.base[[1]]
    fit$rank <- fit.base[[1]]$rank
    fit$n <- length(fit.base[[2]])
    fit$df.residual <- fit$n - fit$rank
    fit$tstat <- fit$coefficients/sqrt(diag(chol2inv(fit$qr$qr)) * 
                                           sum(fit$residuals^2)/(fit$n - fit$rank))
    
    if (method == "qap"){
      xsel <- matrix(TRUE, n, n)
      if (!diag) 
        diag(xsel) <- FALSE
      if (directed == "graph") 
        xsel[upper.tri(xsel)] <- FALSE
      repdist <- matrix(0, times, nx)
      for (i in 1:nx) {
        xfit <- gfit(g[1 + c(i, (1:nx)[-i])], 
                     directed = directed, 
                     diag = diag, rety = TRUE)
        xres <- g[[1 + i]]
        xres[xsel] <- qr.resid(xfit[[1]], xfit[[2]])
        if (directed == "graph")
          xres[upper.tri(xres)] <- t(xres)[upper.tri(xres)]
        repdist[,i] <- purrr::map_dbl(1:times, function(j){
          gfit(c(g[-(1 + i)],
                 list(generate_permutation(xres, with_attr = FALSE))),
               directed = directed, diag = diag,
               rety = FALSE)[nx]
          })
      }
      fit$dist <- repdist
      fit$pleeq <- apply(sweep(fit$dist, 2, fit$tstat, "<="), 
                         2, mean)
      fit$pgreq <- apply(sweep(fit$dist, 2, fit$tstat, ">="), 
                         2, mean)
      fit$pgreqabs <- apply(sweep(abs(fit$dist), 2, abs(fit$tstat), 
                                  ">="), 2, mean)
      fit$nullhyp <- "QAP-DSP"
      fit$names <- names(matrixList)[-1]
      fit$intercept <- intercept
      class(fit) <- "netlm"
      fit  
      
    }
      
  }
  
}

###################

gettval <- function(x, y, tol) {
  xqr <- qr(x, tol = tol)
  coef <- qr.coef(xqr, y)
  resid <- qr.resid(xqr, y)
  rank <- xqr$rank
  n <- length(y)
  rdf <- n - rank
  resvar <- sum(resid^2)/rdf
  cvm <- chol2inv(xqr$qr)
  se <- sqrt(diag(cvm) * resvar)
  coef/se
}

gfit <- function(glist, directed, diag, rety) {
  z <- as.matrix(vectorise_list(glist, simplex = !diag, 
                                directed = (directed == "digraph")))
  if (!rety) {
    gettval(z[,2:ncol(z)], z[,1], tol = 1e-07)
  }
  else {
    list(qr(z[,2:ncol(z)], tol = 1e-07), z[,1])
  }
}

vectorise_list <- function(glist, simplex, directed){
  if(missing(simplex)) simplex <- !is_complex(glist[[1]])
  if(missing(directed)) directed <- is_directed(glist[[1]])
  if(simplex)
    diag(glist[[1]]) <- NA
  if(!directed)
    glist[[1]][upper.tri(glist[[1]])] <- NA
  suppressMessages(na.omit(dplyr::bind_cols(purrr::map(glist, function(x) c(x)))))
}

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

#' @importFrom stats as.formula
convertFormula <- function(formula, new_names){
  stats::as.formula(paste(paste(formula[[2]],"~"),
                   paste(paste0("`", names(new_names)[-1], "`"), collapse = " + ")))
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

