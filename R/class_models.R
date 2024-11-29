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
print.netlm <- function(x, ...){
  cat("# Fitted model results\n")
  print(tidy(x))
  cat("\n# Model summary statistics\n")
  print(glance(x))
}

#' @export
print.netlogit <- function(x, ...){
  cat("# Fitted model results\n")
  print(tidy(x))
  cat("\n# Model summary statistics\n")
  print(glance(x))
}

#' @export
plot.netlm <- function(x, ...){
  distrib <- x$dist
  distrib <- as.data.frame(distrib)
  names(distrib) <- x$names
  distrib$obs <- seq_len(nrow(distrib))
  distrib <- stats::reshape(data = distrib, # tidyr::pivot_longer replacement
                     direction = "long",
                     varying = colnames(distrib)[-ncol(distrib)],
                     v.names = "value",
                     times = colnames(distrib)[-ncol(distrib)],
                     timevar = "name")
  rownames(distrib) <- NULL
  distrib$id <- NULL
  distrib <- dplyr::arrange(distrib, obs)
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
  distrib <- stats::reshape(data = distrib, # tidyr::pivot_longer replacement
                     direction = "long",
                     varying = colnames(distrib)[-ncol(distrib)],
                     v.names = "value",
                     times = colnames(distrib)[-ncol(distrib)],
                     timevar = "name")
  rownames(distrib) <- NULL
  distrib$id <- NULL
  distrib <- dplyr::arrange(distrib, obs)
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
