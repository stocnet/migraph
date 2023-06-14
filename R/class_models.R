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
  distrib <- tidyr::pivot_longer(distrib, 
                                 cols = 1:(ncol(distrib)-1))
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
  distrib <- tidyr::pivot_longer(distrib, 
                                 cols = 1:(ncol(distrib)-1))
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

# diff_model ####
make_diff_model <- function(events, report, .data) {
  class(report) <- c("diff_model", class(report))
  attr(report, "events") <- events
  attr(report, "mode") <- manynet::node_mode(.data)
  report
}

make_diffs_model <- function(report, .data) {
  class(report) <- c("diffs_model", class(report))
  attr(report, "mode") <- manynet::node_mode(.data)
  report
}

#' @export
print.diff_model <- function(x, ...){
  x <- x[,colSums(x, na.rm=TRUE) != 0]
  x$I_new <- NULL
  print(dplyr::tibble(x, ...))
}

#' @export
print.diffs_model <- function(x, ...){
  x <- x[,colSums(x, na.rm=TRUE) != 0]
  x$I_new <- NULL
  print(dplyr::tibble(x, ...))
}

#' @export
summary.diff_model <- function(object, ...){
  dplyr::tibble(attr(object, "events"), ...)
}

#' @export
summary.diffs_model <- function(object, ...){
  sim <- fin <- NULL
  object %>% dplyr::mutate(fin = (I!=n)*1) %>% 
    dplyr::group_by(sim) %>% dplyr::summarise(toa = sum(fin)+1)
}

#' @importFrom dplyr left_join
#' @importFrom ggplot2 geom_histogram
#' @export
plot.diff_model <- function(x, ...){
  if(nrow(x)==1) warning("No diffusion observed.") else {
    S <- E <- I <- I_new <- R <- NULL # initialize variables to avoid CMD check notes
    data <- x
    p <- ggplot2::ggplot(data) + 
      ggplot2::geom_line(ggplot2::aes(x = t, y = S/n, color = "A"),size = 1.25) +
      ggplot2::geom_line(ggplot2::aes(x = t, y = I/n, color = "C"),size = 1.25) +
      ggplot2::geom_col(ggplot2::aes(x = t, y = I_new/n), 
                        alpha = 0.4) +
      ggplot2::theme_minimal() + ggplot2::ylim(0,1) +
      ggplot2::ylab("Proportion") + ggplot2::xlab("Steps")
    if(any(data$E>0))
      p <- p +
      ggplot2::geom_line(ggplot2::aes(x = t, y = E/n, color = "B"),size = 1.25)
    if(any(data$R>0))
      p <- p +
      ggplot2::geom_line(ggplot2::aes(x = t, y = R/n, color = "D"),size = 1.25)
    p + ggplot2::scale_color_manual("Legend", 
                            labels = c("Susceptible", "Exposed", "Infected", "Recovered"),
                            values = c(A = "blue", B = "orange", 
                                       C = "red", D = "darkgreen"),
                            guide = "legend")
  }
}

#' @export
plot.diffs_model <- function(x, ...){
  S <- E <- I <- R <- NULL # initialize variables to avoid CMD check notes
  data <- dplyr::tibble(x)
    # ggplot2::ggplot(data) + geom_smooth()
    
    p <- ggplot2::ggplot(data) + 
      # ggplot2::geom_point(ggplot2::aes(x = t, y = S/n))
      ggplot2::geom_smooth(ggplot2::aes(x = t, y = S/n, color = "A"), 
                           method = "loess", se=TRUE, level = .95, formula = 'y~x') +
      ggplot2::geom_smooth(ggplot2::aes(x = t, y = I/n, color = "C"), 
                           method = "loess", se=TRUE, level = .95, formula = 'y~x') +
      ggplot2::theme_minimal() + ggplot2::ylim(0,1) +
      ggplot2::ylab("Proportion") + ggplot2::xlab("Steps")
    if(any(data$E>0))
      p <- p +
      ggplot2::geom_smooth(ggplot2::aes(x = t, y = E/n, color = "B"), 
                           method = "loess", se=TRUE, level = .95, formula = 'y~x')
    if(any(data$R>0))
      p <- p +
      ggplot2::geom_smooth(ggplot2::aes(x = t, y = R/n, color = "D"), 
                           method = "loess", se=TRUE, level = .95, formula = 'y~x')
    p + ggplot2::scale_color_manual("Legend", 
                                    labels = c("Susceptible", "Exposed", "Infected", "Recovered"),
                                    values = c(A = "blue", B = "orange", 
                                               C = "red", D = "darkgreen"),
                                    guide = "legend")
}

# learn_model ####
make_learn_model <- function(out, .data) {
  out <- as.data.frame(out)
  if(manynet::is_labelled(.data))
    names(out) <- manynet::node_names(.data)
  class(out) <- c("learn_model", class(out))
  attr(out, "mode") <- manynet::node_mode(.data)
  out
}

#' @export
print.learn_model <- function(x, ...){
  print(dplyr::tibble(x))
}

#' @export
summary.learn_model <- function(object, ..., epsilon = 0.0005){
  steps <- nrow(object)
  max_belief <- max(object[steps,])
  min_belief <- min(object[steps,])
  if(abs(max_belief - min_belief) < epsilon){
    cat(paste(steps-1, 
              "steps to convergence.\n"))
    cat("Final belief =", max_belief)
  } else 
    cat(paste("No convergence after",
                  steps-1, "steps."))
}

#' @export
plot.learn_model <- function(x, ...){
  Step <- NULL
  Freq <- NULL
  Var1 <- NULL
  y <- t(x)
  colnames(y) <- paste0("t",0:(ncol(y)-1))
  y <- as.data.frame.table(y)
  y$Step <- as.numeric(gsub("t", "", y$Var2))
  ggplot2::ggplot(y, ggplot2::aes(x = Step, y = Freq, color = Var1)) + 
    ggplot2::geom_line(show.legend = FALSE) + ggplot2::theme_minimal() +
    ggplot2::ylab("Belief")
}


