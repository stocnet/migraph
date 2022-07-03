#' Linear and logistic regression for network data
#' 
#' This function provides an implementation of
#' the multiple regression quadratic assignment procedure (MRQAP)
#' for both one-mode and two-mode network linear models.
#' It offers several advantages:
#' - it works with combined graph/network objects such as igraph and network objects
#'   by constructing the various dependent and independent matrices for the user.
#' - it uses a more intuitive formula-based system for specifying the model,
#'   with several ways to specify how nodal attributes should be handled.
#' - it can handle categorical variables (factors/characters) and
#'   interactions intuitively.
#' - it relies on [`{furrr}`](https://furrr.futureverse.org) for parallelising 
#'   and [`{progressr}`](https://progressr.futureverse.org) 
#'   for reporting progress to the user,
#'   which can be useful when many simulations are required.
#' - results are [`{broom}`](https://broom.tidymodels.org)-compatible, 
#'   with `tidy()` and `glance()` reports to facilitate comparison
#'   with results from different models.
#' Note that a t- or z-value is always used as the test statistic,
#' and properties of the dependent network 
#' -- modes, directedness, loops, etc --
#' will always be respected in permutations and analysis.
#' @name regression
#' @family models
#' @param formula A formula describing the relationship being tested.
#'   Several additional terms are available to assist users investigate
#'   the effects they are interested in. These include:
#'   - `ego()` constructs a matrix where the cells reflect the value of
#'   a named nodal attribute for an edge's sending node
#'   - `alter()` constructs a matrix where the cells reflect the value of
#'   a named nodal attribute for an edge's receiving node
#'   - `same()` constructs a matrix where a 1 reflects 
#'   if two nodes' attribute values are the same
#'   - `dist()` constructs a matrix where the cells reflect the
#'   absolute difference between the attribute's values 
#'   for the sending and receiving nodes
#'   - `sim()` constructs a matrix where the cells reflect the
#'   proportional similarity between the attribute's values 
#'   for the sending and receiving nodes
#'   - dyadic covariates (other networks) can just be named
#' @inheritParams is
#' @param method A method for establishing the null hypothesis.
#'   Note that "qap" uses Dekker et al's (2007) double semi-partialling technique,
#'   whereas "qapy" permutes only the $y$ variable.
#'   "qap" is the default.
#' @param times Integer indicating number of simulations used for quantile estimation. 
#'   (Relevant to the null hypothesis test only - 
#'   the analysis itself is unaffected by this parameter.) 
#'   Note that, as for all Monte Carlo procedures, convergence is slower for more
#'   extreme quantiles.
#'   By default, `times=1000`.
#'   1,000 - 10,000 repetitions recommended for publication-ready results.
#' @param strategy If `{furrr}` is installed, 
#'   then multiple cores can be used to accelerate the function.
#'   By default `"sequential"`, 
#'   but if multiple cores available,
#'   then `"multisession"` or `"multicore"` may be useful.
#'   Generally this is useful only when `times` > 1000.
#'   See [`{furrr}`](https://furrr.futureverse.org) for more.
#' @param verbose Whether the function should report on its progress.
#'   By default FALSE.
#'   See [`{progressr}`](https://progressr.futureverse.org) for more.
#' @importFrom dplyr bind_cols left_join
#' @importFrom purrr flatten
#' @importFrom future plan
#' @importFrom furrr future_map_dfr furrr_options
#' @importFrom stats glm.fit as.formula df.residual pchisq
#' @seealso `vignette("p7linearmodel")`
#' @references 
#'   Krackhardt, David. 1988.
#'   “Predicting with Networks: Nonparametric Multiple Regression Analysis of Dyadic Data.” 
#'   _Social Networks_ 10(4):359–81.
#'   \doi{10.1016/0378-8733(88)90004-4}.
#'   
#'   Dekker, David, David Krackhard, and Tom A. B. Snijders. 2007.
#'   “Sensitivity of MRQAP tests to collinearity and autocorrelation conditions.”
#'   _Psychometrika_ 72(4): 563-581.
#'   \doi{10.1007/s11336-007-9016-1}.
#'   
#' @examples
#' networkers <- ison_networkers %>% to_subgraph(Discipline == "Sociology")
#' model1 <- network_reg(weight ~ alter(Citations) + sim(Citations), 
#'                       networkers, times = 20)
#' # Should be run many more `times` for publication-ready results
#' tidy(model1)
#' glance(model1)
#' plot(model1)
#' @export
network_reg <- function(formula, object,
                        method = c("qap","qapy"),
                        times = 1000,
                        strategy = "sequential",
                        verbose = FALSE) {
  
  # Setup ####
  matrixList <- convertToMatrixList(formula, object)
  convForm <- convertFormula(formula, matrixList)
  
  method <- match.arg(method)

  g <- matrixList
  nx <- length(matrixList) - 1
  n <- dim(matrixList[[1]])
  
  directed <- ifelse(is_directed(matrixList[[1]]), "digraph", "graph")
  valued <- is_weighted(matrixList[[1]])
  diag <- is_complex(matrixList[[1]])
  
  if (any(vapply(lapply(g, is.na), any, 
                 FUN.VALUE = logical(1)))) 
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
  # qapy for univariate ####
  if (method == "qapy" | nx == 2){
    future::plan(strategy)
    if(valued){
      repdist <- furrr::future_map_dfr(1:times, function(j){
        nlmfit(c(list(generate_permutation(g[[1]], with_attr = FALSE)),
                 g[2:(nx+1)]),
               directed = directed, diag = diag,
               rety = FALSE)
      }, .progress = verbose, .options = furrr::furrr_options(seed = T))
    } else {
      repdist <- furrr::future_map_dfr(1:times, function(j){
        repfit <- nlgfit(c(list(generate_permutation(g[[1]], with_attr = FALSE)),
                           g[2:(nx+1)]),
                         directed = directed, diag = diag)
        repfit$coef/sqrt(diag(chol2inv(repfit$qr$qr)))
      }, .progress = verbose, .options = furrr::furrr_options(seed = T))
    }
    # qapspp for multivariate ####
  } else if (method == "qap"){
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
      
      future::plan(strategy)
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
    }
  }

  fit$dist <- repdist
  fit$pleeq <- apply(sweep(fit$dist, 2, fit$tstat, "<="), 
                     2, mean)
  fit$pgreq <- apply(sweep(fit$dist, 2, fit$tstat, ">="), 
                     2, mean)
  fit$pgreqabs <- apply(sweep(abs(fit$dist), 2, abs(fit$tstat), 
                              ">="), 2, mean)
  if(method == "qapy" | nx == 2) 
    fit$nullhyp <- "QAPy"
  else fit$nullhyp <- "QAP-DSP"
  fit$names <- names(matrixList)[-1]
  fit$intercept <- TRUE
  if(valued) 
    class(fit) <- "netlm"
  else 
    class(fit) <- "netlogit"
  fit  
  
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

#' @importFrom stats binomial
nlgfit <- function(glist, directed, diag) {
  z <- as.matrix(vectorise_list(glist, simplex = !diag, 
                                directed = (directed == "digraph")))
  stats::glm.fit(z[,2:ncol(z)], z[,1], 
                 family = stats::binomial(), intercept = FALSE)
}

vectorise_list <- function(glist, simplex, directed){
  if(missing(simplex)) simplex <- !is_complex(glist[[1]])
  if(missing(directed)) directed <- is_directed(glist[[1]])
  if(simplex)
    diag(glist[[1]]) <- NA
  if(!directed)
    glist[[1]][upper.tri(glist[[1]])] <- NA
  suppressMessages(stats::na.omit(dplyr::bind_cols(furrr::future_map(glist, 
                                                       function(x) c(x)))))
}

convertToMatrixList <- function(formula, data){
  data <- as_tidygraph(data)
  DV <- as_matrix(to_uniplex(data, 
                             edge = getDependentName(formula)))
  IVnames <- getRHSNames(formula)
  IVs <- lapply(IVnames, function(IV){
    out <- lapply(seq_along(IV), function(elem){
      if(IV[[elem]][1] == "ego"){
        vct <- node_attribute(data, IV[[elem]][2])
        if(is.character(vct) | is.factor(vct)){
          fct <- factor(vct)
          if(length(levels(fct)) == 2){
            out <- matrix(as.numeric(fct)-1,
                          nrow(DV), ncol(DV))
            names(out) <- paste(IV[[elem]], collapse = " ")
            out <- out
          } else {
            out <- lapply(2:length(levels(fct)),
                          function (x) matrix((as.numeric(fct)==x)*1,
                                              nrow(DV), ncol(DV)))
            names(out) <- paste(paste(IV[[elem]], collapse = " "), 
                                levels(fct)[2:length(levels(fct))])
            out <- out
          }
        } else {
          out <- matrix(vct, nrow(DV), ncol(DV))
          out <- list(out)
          names(out) <- paste(IV[[elem]], collapse = " ")
          out <- out
        }
      } else if (IV[[elem]][1] == "alter"){
          vct <- node_attribute(data, IV[[elem]][2])
          if(is.character(vct) | is.factor(vct)){
            fct <- factor(vct)
            if(length(levels(fct)) == 2){
              out <- matrix(as.numeric(fct)-1,
                            nrow(DV), ncol(DV))
              names(out) <- paste(IV[[elem]], collapse = " ")
              out <- out
            } else {
              out <- lapply(2:length(levels(fct)),
                            function (x) matrix((as.numeric(fct)==x)*1,
                                                nrow(DV), ncol(DV)))
              names(out) <- paste(paste(IV[[elem]], collapse = " "), 
                                  levels(fct)[2:length(levels(fct))])
              out <- out
            }
          } else {
            out <- matrix(vct, nrow(DV), ncol(DV), byrow = TRUE)
            out <- list(out)
            names(out) <- paste(IV[[elem]], collapse = " ")
            out <- out
          }
      } else if (IV[[elem]][1] == "same"){
        rows <- matrix(node_attribute(data, IV[[elem]][2]),
                       nrow(DV), ncol(DV))
        cols <- matrix(node_attribute(data, IV[[elem]][2]),
                       nrow(DV), ncol(DV), byrow = TRUE)
        out <- (rows==cols)*1
        out <- list(out)
        names(out) <- paste(IV[[elem]], collapse = " ")
        out <- out
      } else if (IV[[elem]][1] == "dist"){
        if(is.character(node_attribute(data, IV[[elem]][2])))
          stop("Distance undefined for factors.")
        rows <- matrix(node_attribute(data, IV[[elem]][2]),
                       nrow(DV), ncol(DV))
        cols <- matrix(node_attribute(data, IV[[elem]][2]),
                       nrow(DV), ncol(DV), byrow = TRUE)
        out <- abs(rows - cols)
        out <- list(out)
        names(out) <- paste(IV[[elem]], collapse = " ")
        out <- out
      } else if (IV[[elem]][1] == "sim"){
        if(is.character(node_attribute(data, IV[[elem]][2])))
          stop("Similarity undefined for factors.")
        rows <- matrix(node_attribute(data, IV[[elem]][2]),
                       nrow(DV), ncol(DV))
        cols <- matrix(node_attribute(data, IV[[elem]][2]),
                       nrow(DV), ncol(DV), byrow = TRUE)
        out <- abs(1- abs(rows - cols)/max(abs(rows - cols)))
        out <- list(out)
        names(out) <- paste(IV[[elem]], collapse = " ")
        out <- out
      } else {
        if (IV[[elem]][1] %in% graph_tie_attributes(data)){
          out <- as_matrix(to_uniplex(data, 
                                      edge = IV[[elem]][1]))
          out <- list(out)
          names(out) <- IV[[elem]][1]
          out <- out
        }
      }
    })
    if(length(out)==2){
      namo <- paste(vapply(out, names, FUN.VALUE = character(1)), 
                    collapse = ":")
      out <- list(out[[1]][[1]] * out[[2]][[1]])
      names(out) <- namo
      out
    } else {
      if(is.list(out[[1]]))
        out[[1]] else{
          list(out[[1]])
        } 
    }})
  IVs <- purrr::flatten(IVs)
  out <- c(list(DV), list(matrix(1, dim(DV)[1], dim(DV)[2])), IVs)
  names(out)[1:2] <- c(formula[[2]], "(intercept)")
  out
}

convertFormula <- function(formula, new_names){
  stats::as.formula(paste(paste(formula[[2]],"~"),
                   paste(paste0("`", names(new_names)[-1], "`"), collapse = " + ")))
}

getRHSNames <- function(formula) {
  rhs <- c(attr(stats::terms(formula), "term.labels"))
  rhs <- strsplit(rhs, ":")
  # embed single parameter models in list
  if (!is.list(rhs)) rhs <- list(rhs)
  lapply(rhs, function(term) strsplit(gsub("\\)", "", term), "\\("))
}

getDependentName <- function(formula) {
  dep <- list(formula[[2]])
  depName <- unlist(lapply(dep, deparse))
}

#' @importFrom generics tidy
#' @export
generics::tidy

#' @method tidy netlm
#' @importFrom stats quantile
#' @export
tidy.netlm <- function(x, conf.int = FALSE, conf.level = 0.95, ...) {
  
  result <- dplyr::tibble(term = x$names,
                           estimate = x$coefficients,
                           # std.error = NA_real_,
                           statistic = x$tstat,
                           p.value = x$pgreqabs)
  
  if (conf.int) {
    ci <- apply(x$dist, 2, stats::quantile, c(.025, .975))
    ci <- cbind(data.frame(term = x$names), t(ci))
    names(ci) <- c("term", "conf.low", "conf.high")
    result <- dplyr::left_join(result, ci, by = "term")
  }
  
  result
}

#' @method tidy netlogit
#' @importFrom stats quantile
#' @export
tidy.netlogit <- function(x, conf.int = FALSE, conf.level = 0.95, 
                          exponentiate = FALSE, ...) {
  
  result <- dplyr::tibble(term = x$names,
                           estimate = `if`(exponentiate, 
                                             exp(x$coefficients), 
                                             x$coefficients),
                           # std.error = NA_real_,
                           statistic = x$tstat,
                           p.value = x$pgreqabs)
  
  if (conf.int) {
    ci <- apply(x$dist, 2, stats::quantile, c(.025, .975))
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
  
  mss <- sum((stats::fitted(x) - mean(stats::fitted(x)))^2)
  rss <- sum(stats::resid(x)^2)
  qn <- NROW(x$qr$qr)
  df.int <- x$intercept
  rdf <- qn - x$rank
  resvar <- rss/rdf
  fstatistic <- c(value = (mss/(x$rank - df.int))/resvar, 
                  numdf = x$rank - df.int, 
                  dendf = rdf)
  r.squared <- mss/(mss + rss)
  adj.r.squared <- 1 - (1 - r.squared) * ((qn - df.int)/rdf)
  sigma <- sqrt(resvar)
  
    dplyr::tibble(
      r.squared = r.squared,
      adj.r.squared = adj.r.squared,
      sigma = sigma,
      statistic = fstatistic["value"],
      p.value = stats::pf(
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
  
  dplyr::tibble(
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
    chi.squared = 1 - stats::pchisq(x$null.deviance - x$deviance, 
                                    df = x$df.null - x$df.residual),
    deviance = x$deviance,
    null.deviance = x$null.deviance,
    df.residual = stats::df.residual(x),
    nobs = x$n
  )
}

#' @export
plot.netlm <- function(x, ...){
  distrib <- x$dist
  distrib <- as.data.frame(distrib)
  names(distrib) <- x$names
  distrib$obs <- seq_len(nrow(distrib))
  distrib <- tidyr::pivot_longer(distrib, -.data$obs)
  distrib$coef <- rep(unname(x$coefficients), nrow(x$dist))
  distrib$tstat <- rep(unname(x$tstat), nrow(x$dist))
  distrib$name <- factor(distrib$name, x$names)
  ggplot2::ggplot(distrib, ggplot2::aes(.data$value, .data$name)) + 
    ggplot2::geom_violin(draw_quantiles = c(0.025, 0.975)) + 
    ggplot2::theme_minimal() +
    ylab("") + xlab("Statistic") + 
    ggplot2::geom_point(aes(x = .data$tstat), size = 2, 
                        colour = "red") +
    scale_y_discrete(limits=rev)
}

#' @export
plot.netlogit <- function(x, ...){
  distrib <- x$dist
  distrib <- as.data.frame(distrib)
  names(distrib) <- x$names
  distrib$obs <- seq_len(nrow(distrib))
  distrib <- tidyr::pivot_longer(distrib, -.data$obs)
  distrib$coef <- rep(unname(x$coefficients), nrow(x$dist))
  distrib$tstat <- rep(unname(x$tstat), nrow(x$dist))
  distrib$name <- factor(distrib$name, x$names)
  ggplot2::ggplot(distrib, ggplot2::aes(.data$value, .data$name)) + 
    ggplot2::geom_violin(draw_quantiles = c(0.025, 0.975)) + 
    ggplot2::theme_minimal() +
    ylab("") + xlab("Statistic") + 
    ggplot2::geom_point(aes(x = .data$tstat), size = 2, 
               colour = "red") +
    scale_y_discrete(limits=rev)
}
