#' Linear regression for multimodal network data
#' 
#' This function extends the multiple regression quadratic assignment procedure (MRQAP)
#' of network linear model to two mode networks. 
#' @name netlm
#' @param formula A formula describing the relationship being tested.
#' @param data Expects a list of matrices, graphs, or a tidygraph object.
#' @param reps Integer indicating the number of draws to use for quantile estimation. 
#' (Relevant to the null hypothesis test only - the analysis itself is unaffected by this parameter.) 
#' Note that, as for all Monte Carlo procedures, convergence is slower for more extreme quantiles. 
#' By default, reps=1000.
#' @export
netlm <- function(formula, data, reps = 1000){
  out <- lm(formula, data)
  class(out) <- "netlm"
  out
}

#' @rdname netlm
print.summary.netlm <- function(){
  
}

#' @param DV A two-mode matrix object
#' @param IV A list of two-mode matrix objects
#' @param names IV names
#' @param rep Number of permutations. Default: 1000.
#' @importFrom stats ecdf lm
#' @examples
#' \dontrun{
#' netlm2(matrix, list(matrix2, matrix3),
#'               c("Explanatory Variable", "Control Variable"))
#' }
#' 
#' 
netlm2 <- function(formula, data, names, rep = 1000){
  
  #Could be automated by extracting the names of the named selected list of dependent variables DV. (after selection)
  if(missing(names)){ 
    names <- paste0("x", 1:length(IV))
  }
  # Decomposing the formula into its components.
  
  formula <- as.formula(formula)
  tn <- as.character(formula[[1]]) # ~
  yn <- as.character(formula[[2]]) # IV
  xn <- deparse(formula[[3]]) 
  xn <- c(unlist(strsplit(xn, split = " ")))
  xn <- as.vector(xn[xn != "+"])
  
  
  #Selecting the matrices in the data list.
  IV <- data %>% keep(names(.) %in% xn)
  DV <- purrr::pluck(data, yn)

  #Permutation, list of matrices.
  rbperm <- function (m) {
    n <- sample(1:dim(m)[1])
    o <- sample(1:dim(m)[2])
    p <- matrix(data = m[n, o], nrow = dim(m)[1], ncol = dim(m)[2])
    p
  }

  nIV <- length(IV)
  M.fit <- lm(as.numeric(unlist(DV)) ~ Reduce(cbind,
                                              lapply(1:length(IV), function(x) unlist(IV[x][1]))))
  M.coeff <- M.fit$coefficients

  permDist <- matrix(0, rep, (nIV+1))

  for(i in 1:rep){
    tempDV <- rbperm(DV)
    permDist[i,] <- (lm(as.numeric(unlist(tempDV)) ~
                          Reduce(cbind,lapply(1:length(IV),
                                              function(x) unlist(IV[x][1])))))$coefficients
  }

  resTable <- data.frame(Effect = c("Intercept", names),
                         Coefficients = formatC(M.coeff, format = "f", digits = 2),
                         Pvalue = signif(as.numeric(lapply(1:(nIV+1),
                                                           function(x) ecdf(permDist[,x])(M.coeff[x]))),
                                         digits = 2),
                         Sig = ifelse(as.numeric(lapply(1:(nIV+1),
                                                        function(x) ecdf(permDist[,x])(M.coeff[x])))<0.05,
                                      ifelse(as.numeric(lapply(1:(nIV+1),
                                                               function(x) ecdf(permDist[,x])(M.coeff[x])))<0.01,
                                             ifelse(as.numeric(lapply(1:(nIV+1),
                                                                      function(x) ecdf(permDist[,x])(M.coeff[x])))<0.001,
                                                    "***", "**"), "*"), ""))
  rownames(resTable) <- NULL
  print(resTable)
  # Turn this into a print function

  cat("\nMultiple R-squared: ", formatC(summary(M.fit)$r.squared),
      ",\tAdjusted R-squared: ", formatC(summary(M.fit)$adj.r.squared),
      "\n", sep="")

  obj <- list()
  obj$results <- data.frame(Effect = c("Intercept", names),
                            Coefficients = as.numeric(formatC(M.coeff, format="f", digits = 2)),
                            Pvalue = signif(as.numeric(lapply(1:(nIV+1),
                                                              function(x) ecdf(permDist[,x])(M.coeff[x]))),
                                            digits=2),
                            Sig = ifelse(as.numeric(lapply(1:(nIV+1),
                                                           function(x) ecdf(permDist[,x])(M.coeff[x])))<0.05,
                                         ifelse(as.numeric(lapply(1:(nIV+1),
                                                                  function(x) ecdf(permDist[,x])(M.coeff[x])))<0.01,
                                                ifelse(as.numeric(lapply(1:(nIV+1),
                                                                         function(x) ecdf(permDist[,x])(M.coeff[x])))<0.001,
                                                       "***", "**"), "*"), ""))
  rownames(obj$results) <- NULL
  obj$r.squared <- formatC(summary(M.fit)$r.squared)
  obj$adj.r.squared <- formatC(summary(M.fit)$adj.r.squared)
  invisible(obj)
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
