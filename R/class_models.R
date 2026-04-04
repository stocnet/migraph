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

#' @method tidy ergm
#' @importFrom stats quantile
#' @export
tidy.ergm <- function(
    x,
    conf.int = FALSE,
    conf.level = 0.95,
    exponentiate = FALSE,
    ...
) {
  # in ergm 3.9 summary(x, ...)$coefs has columns:
  #   Estimate, Std. Error, MCMC %, Pr(>|Z|)
  
  # in ergm 3.10 summary(x, ...)$coefs has columns:
  #   Estimate, Std. Error, MCMC %, z value, Pr(>|Z|)
  
  ret <- summary(x, ...)$coefficients %>%
    dplyr::as_tibble(rownames = "term") %>%
    rename2(
      term = "term",
      estimate = "Estimate",
      std.error = "Std. Error",
      mcmc.error = "MCMC %",
      statistic = "z value",
      p.value = "Pr(>|z|)"
    )
  
  if (conf.int) {
    z <- stats::qnorm(1 - (1 - conf.level) / 2)
    ret$conf.low <- ret$estimate - z * ret$std.error
    ret$conf.high <- ret$estimate + z * ret$std.error
  }
  
  if (exponentiate) {
    if (
      is.null(x$glm) ||
      (x$glm$family$link != "logit" && x$glm$family$link != "log")
    ) {
      manynet::snet_warn(
        "Coefficients will be exponentiated, but the model didn't 
         use a {.code log} or {.code logit} link."
      )
    }
    
    ret <- exponentiate(ret)
  }
  
  dplyr::as_tibble(ret)
}

#' @method tidy sienaFit
#' @importFrom dplyr tibble
#' @export
tidy.sienaFit <- function(x, ...){
  dplyr::tibble(
    dv = x$effects$name,
    term = x$effects$effectName,
    estimate = x$theta,
    std.error = x$se,
    statistic = x$theta/x$se
  )
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


#' @method glance ergm
#' @importFrom ergm as.rlebdm
#' @export
glance.ergm <- function(x, deviance = FALSE, mcmc = FALSE, ...) {
  s <- summary(x, ...) # produces lots of messages
  
  ret <- dplyr::tibble(
    independence = s$independence,
    iterations = x$iterations,
    logLik = as.numeric(stats::logLik(x))
  )
  
  if (deviance & !is.null(ret$logLik)) {
    # see #567 for details on the following
    
    thisRequires("ergm")
    
    if (utils::packageVersion("ergm") < "3.10") {
      dyads <- sum(
        ergm::as.rlebdm(x$constrained, x$constrained.obs, which = "informative")
      )
    } else {
      dyads <- stats::nobs(x)
    }
    
    lln <- ergm::logLikNull(x)
    ret$null.deviance <- if (is.na(lln)) 0 else -2 * lln
    ret$df.null <- dyads
    
    ret$residual.deviance <- -2 * ret$logLik
    ret$df.residual <- dyads - length(x$coefs)
  }
  
  ret$AIC <- stats::AIC(x)
  ret$BIC <- stats::BIC(x)
  
  if (mcmc) {
    if (isTRUE(x$MPLE_is_MLE)) {
      manynet::snet_info(
        c(
          "Though {.fn glance} was supplied {.code mcmc = TRUE}, the model was not
           fitted using MCMC,",
          "i" = "The corresponding columns will be omitted."
        )
      )
    }
    
    ret$MCMC.interval <- x$control$MCMC.interval
    ret$MCMC.burnin <- x$control$MCMC.burnin
    ret$MCMC.samplesize <- x$control$MCMC.samplesize
  }
  
  ret
}

#' @method glance sienaFit
#' @export
glance.sienaFit <- function(x, ...){
  dplyr::tibble(
    tmax = x$tmax,
    tconv.max = x$tconv.max[,1]
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

# Unused because infinite recursion through summary.ergm() in tidy.ergm()
# #' @export
# summary.ergm <- function(x, ...){
#   cat("# Fitted model results\n")
#   print(tidy(x))
#   cat("\n# Model summary statistics\n")
#   print(glance(x))
# }

#' @export
summary.sienaFit <- function(object, ...){
  cat("# Fitted model results\n")
  print(tidy(object))
  cat("\n# Model summary statistics\n")
  print(glance(object))
}


# Utilities from broom ####

rename2 <- function(.data, ...) {
  dots <- dplyr::quos(...)
  present <- purrr::keep(dots, ~ dplyr::quo_name(.x) %in% colnames(.data))
  dplyr::rename(.data, !!!present)
}

exponentiate <- function(data, col = "estimate") {
  data <- data %>% dplyr::mutate(dplyr::across(dplyr::all_of(col), exp))
  
  if ("conf.low" %in% colnames(data)) {
    data <- data %>% dplyr::mutate(dplyr::across(c(conf.low, conf.high), exp))
  }
  
  data
}
