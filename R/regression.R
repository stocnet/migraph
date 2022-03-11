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
#'   Several additional terms are available to assist users investigate
#'   the effects they are interested in. These include:
#'   - `ego()` tests whether a nodal attribute relates to outgoing ties
#'   - `alter()` tests whether a nodal attribute relates to incoming ties
#'   - `same()` adds a 1 if two nodes' attribute values are the same
#' @param data An igraph, network, or tidygraph object.
#' @param method A method for establishing the null hypothesis.
#'   Note that "qap" currently defaults to Dekker et al's double semi-partialling technique.
#' @param times Integer indicating the number of draws to use for quantile
#'   estimation. (Relevant to the null hypothesis test only - the analysis
#'   itself is unaffected by this parameter.) 
#'   Note that, as for all Monte Carlo procedures, convergence is slower for more
#'   extreme quantiles.
#'   By default, times=1000.
#'   1,000 - 10,000 repetitions recommended for publication-ready results.
#' @param parallel If `{furrr}` is installed, and the number of simulations is high,
#'   current thinking is `times >= 1000`, 
#'   then multiple cores can be used to accelerate the function.
#' @param verbose If `parallel` is TRUE, 
#'   should the function also report on its progress.
#' @importFrom dplyr bind_cols
#' @importFrom purrr map
#' @importFrom stats lm
#' @references 
#'   Dekker, D., Krackhard, D., Snijders, T.A.B (2007) 
#'   Sensitivity of MRQAP tests to collinearity and autocorrelation conditions. 
#'   Psychometrika 72(4): 563-581.
#' @examples
#' messages <- mutate_edges(ison_eies, 
#'   generate_random(ison_eies), attr_name = "random")
#' (model1 <- network_reg(weight ~ random + 
#'   same(Discipline) + same(Citations), messages, times = 500))
#' messaged <- messages %>% activate(edges) %>% 
#'    tidygraph::mutate(weight = (weight > 0)*1)
#' (model2 <- network_reg(weight ~ random + 
#'   same(Discipline) + same(Citations), messaged, times = 500))
#' tidy(model1)
#' tidy(model2, exponentiate = TRUE)
#' glance(model1)
#' glance(model2)
#' @export
network_reg <- function(formula, data,
                        method = c("qap","qapy"),
                        times = 1000,
                        parallel = FALSE,
                        verbose = FALSE) {
  
  # Setup ####
  matrixList <- convertToMatrixList(formula, data)
  convForm <- convertFormula(formula, matrixList)
  
  method <- match.arg(method)

  g <- matrixList
  nx <- length(matrixList) - 1
  n <- dim(matrixList[[1]])
  
  directed <- ifelse(is_directed(matrixList[[1]]), "digraph", "graph")
  valued <- is_weighted(matrixList[[1]])
  diag <- is_complex(matrixList[[1]])
  
  if (any(sapply(lapply(g, is.na), any))) 
    stop("Missing data supplied; this may pose problems for certain null hypotheses.")
  
  # Base ####
  if(valued){
    fit.base <- nlmfit(g, 
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
  } else {
    fit.base <- nlgfit(g, 
                       directed = directed, 
                       diag = diag)
    fit <- list()
    fit$coefficients <- fit.base$coefficients
    fit$fitted.values <- fit.base$fitted.values
    fit$residuals <- fit.base$residuals
    fit$se <- sqrt(diag(chol2inv(fit.base$qr$qr)))
    fit$tstat <- fit$coefficients/fit$se
    fit$linear.predictors <- fit.base$linear.predictors
    fit$n <- length(fit.base$y)
    fit$df.model <- fit.base$rank
    fit$df.residual <- fit.base$df.residual
    fit$deviance <- fit.base$deviance
    fit$null.deviance <- fit.base$null.deviance
    fit$df.null <- fit.base$df.null
    fit$rank <- fit.base$rank
    fit$aic <- fit.base$aic
    fit$bic <- fit$deviance + fit$df.model * log(fit$n)
    fit$qr <- fit.base$qr
    fit$ctable <- table(as.numeric(fit$fitted.values >= 0.5), 
                        fit.base$y, dnn = c("Predicted", "Actual"))
    if (NROW(fit$ctable) == 1) {
      if (rownames(fit$ctable) == "0") 
        fit$ctable <- rbind(fit$ctable, c(0, 0))
      else fit$ctable <- rbind(c(0, 0), fit$ctable)
      rownames(fit$ctable) <- c("0", "1")
    }
  }

  # Null ####
  if (method == "qap"){
    xsel <- matrix(TRUE, n[1], n[2])
    if (!diag) 
      diag(xsel) <- FALSE
    if (directed == "graph") 
      xsel[upper.tri(xsel)] <- FALSE
    repdist <- matrix(0, times, nx)
    
    for (i in 1:nx) {
      xfit <- nlmfit(g[1 + c(i, (1:nx)[-i])], 
                     directed = directed, 
                     diag = diag, rety = TRUE)
      xres <- g[[1 + i]]
      xres[xsel] <- qr.resid(xfit[[1]], xfit[[2]])
      if (directed == "graph")
        xres[upper.tri(xres)] <- t(xres)[upper.tri(xres)]
      if(parallel & times >= 1000){
        future::plan("multisession")
        if(valued){
          repdist[,i] <- furrr::future_map_dbl(1:times, function(j){
            nlmfit(c(g[-(1 + i)],
                     list(generate_permutation(xres, with_attr = FALSE))),
                   directed = directed, diag = diag,
                   rety = FALSE)[nx]
          }, .progress = verbose, .options = furrr::furrr_options(seed = T))
        } else {
          repdist[,i] <- furrr::future_map_dbl(1:times, function(j){
            repfit <- nlgfit(c(g[-(1 + i)],
                               list(generate_permutation(xres, with_attr = FALSE))),
                             directed = directed, diag = diag)
            repfit$coef[nx]/sqrt(diag(chol2inv(repfit$qr$qr)))[nx]
          }, .progress = verbose, .options = furrr::furrr_options(seed = T))
        }
      } else {
        if(valued){
          repdist[,i] <- purrr::map_dbl(1:times, function(j){
            nlmfit(c(g[-(1 + i)],
                     list(generate_permutation(xres, with_attr = FALSE))),
                   directed = directed, diag = diag,
                   rety = FALSE)[nx]
          })
        } else {
          repdist[,i] <- purrr::map_dbl(1:times, function(j){
            repfit <- nlgfit(c(g[-(1 + i)],
                     list(generate_permutation(xres, with_attr = FALSE))),
                   directed = directed, diag = diag)
            repfit$coef[nx]/sqrt(diag(chol2inv(repfit$qr$qr)))[nx]
          })
        }
      }
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
    fit$intercept <- TRUE
    if(valued) 
      class(fit) <- "netlm"
    else 
      class(fit) <- "netlogit"
    fit  
    
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

nlmfit <- function(glist, directed, diag, rety) {
  z <- as.matrix(vectorise_list(glist, simplex = !diag, 
                                directed = (directed == "digraph")))
  if (!rety) {
    gettval(z[,2:ncol(z)], z[,1], tol = 1e-07)
  }
  else {
    list(qr(z[,2:ncol(z)], tol = 1e-07), z[,1])
  }
}

nlgfit <- function(glist, directed, diag) {
  z <- as.matrix(vectorise_list(glist, simplex = !diag, 
                                directed = (directed == "digraph")))
  stats::glm.fit(z[,2:ncol(z)], z[,1], 
                 family = binomial(), intercept = FALSE)
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
  out <- c(list(DV), list(matrix(1, dim(DV)[1], dim(DV)[2])), IVs)
  names(out) <- c(formula[[2]], "(intercept)",
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

#' @importFrom generics tidy
#' @export
generics::tidy

#' @method tidy netlm
#' @export
tidy.netlm <- function(x, conf.int = FALSE, conf.level = 0.95, ...) {
  
  result <- tibble::tibble(term = x$names,
                           estimate = x$coefficients,
                           std.error = NA_real_,
                           statistic = x$tstat,
                           p.value = x$pgreqabs)
  
  if (conf.int) {
    ci <- apply(x$dist, 2, quantile, c(.025, .975))
    ci <- cbind(data.frame(term = x$names), t(ci))
    names(ci) <- c("term", "conf.low", "conf.high")
    result <- dplyr::left_join(result, ci, by = "term")
  }
  
  result
}

#' @method tidy netlogit
#' @export
tidy.netlogit <- function(x, conf.int = FALSE, conf.level = 0.95, 
                          exponentiate = FALSE, ...) {
  
  result <- tibble::tibble(term = x$names,
                           estimate = `if`(exponentiate, 
                                             exp(x$coefficients), 
                                             x$coefficients),
                           std.error = NA_real_,
                           statistic = x$tstat,
                           p.value = x$pgreqabs)
  
  if (conf.int) {
    ci <- apply(x$dist, 2, quantile, c(.025, .975))
    ci <- cbind(data.frame(term = x$names), t(ci))
    names(ci) <- c("term", "conf.low", "conf.high")
    result <- dplyr::left_join(result, ci, by = "term")
  }
  
  result
}

#' @importFrom generics glance
#' @export
generics::glance

#' @method glance netlm
#' @export
glance.netlm <- function(x, ...) {
  
  mss <- sum((fitted(x) - mean(fitted(x)))^2)
  rss <- sum(resid(x)^2)
  qn <- NROW(x$qr$qr)
  df.int <- x$intercept
  rdf <- qn - x$rank
  resvar <- rss/rdf
  fstatistic <- c(value = (mss/(x$rank - df.int))/resvar, numdf = x$rank - 
                    df.int, dendf = rdf)
  r.squared <- mss/(mss + rss)
  adj.r.squared <- 1 - (1 - r.squared) * ((qn - df.int)/rdf)
  sigma <- sqrt(resvar)
  
    tibble::tibble(
      r.squared = r.squared,
      adj.r.squared = adj.r.squared,
      sigma = sigma,
      statistic = fstatistic["value"],
      p.value = pf(
        fstatistic["value"],
        fstatistic["numdf"],
        fstatistic["dendf"],
        lower.tail = FALSE
      ),
      df = fstatistic["numdf"],
      # logLik = as.numeric(stats::logLik(x)),
      # AIC = stats::AIC(x),
      # BIC = stats::BIC(x),
      # deviance = stats::deviance(x),
      df.residual = stats::df.residual(x),
      nobs = x$n
    )
}

#' @method glance netlogit
#' @export
glance.netlogit <- function(x, ...) {
  
  # mss <- sum((fitted(x) - mean(fitted(x)))^2)
  # rss <- sum(resid(x)^2)
  # qn <- NROW(x$qr$qr)
  # df.int <- x$intercept
  # rdf <- qn - x$rank
  # resvar <- rss/rdf
  # fstatistic <- c(value = (mss/(x$rank - df.int))/resvar, numdf = x$rank - 
  #                   df.int, dendf = rdf)
  # r.squared <- mss/(mss + rss)
  # adj.r.squared <- 1 - (1 - r.squared) * ((qn - df.int)/rdf)
  # sigma <- sqrt(resvar)
  
  tibble::tibble(
    # r.squared = r.squared,
    # adj.r.squared = adj.r.squared,
    # sigma = sigma,
    # statistic = fstatistic["value"],
    # p.value = pf(
    #   fstatistic["value"],
    #   fstatistic["numdf"],
    #   fstatistic["dendf"],
    #   lower.tail = FALSE
    # ),
    # df = fstatistic["numdf"],
    # logLik = as.numeric(stats::logLik(x)),
    pseudo.r.squared = (x$null.deviance - x$deviance)/(x$null.deviance - x$deviance + 
                                                         x$df.null),
    AIC = x$aic,
    AICc = x$aic + (2*x$rank^2 + 2*x$rank)/(x$n-x$rank-1),
    BIC = x$bic,
    chi.squared = 1 - stats::pchisq(x$null.deviance - x$deviance, df = x$df.null - 
                               x$df.residual),
    deviance = x$deviance,
    null.deviance = x$null.deviance,
    df.residual = stats::df.residual(x),
    nobs = x$n
  )
}

