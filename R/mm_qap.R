#' Two-mode quadratic assignment procedure
#'
#' This function allows you to calculate how much two-mode clustering there is.
#' @param DV A two-mode matrix object
#' @param IV A list of two-mode matrix objects
#' @param names IV names
#' @param rep Number of permutations. Default: 1000.
#' @family two-mode functions
#' @export
#' @examples
#' \dontrun{
#' twomode_netlm(matrix, list(matrix2, matrix3),
#'               c("Intercept", "Explainer"))
#' }
twomode_netlm <- function(DV, IV, names, rep = 1000){
  
  # if(missing(names)) names <- c("Intercept", lapply(IV, function(x) names(x))
  # Consider converting to formula
  
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
print(resTable)
  
  cat("\nMultiple R-squared: ", formatC(summary(M.fit)$r.squared),
      ",\tAdjusted R-squared: ", formatC(summary(M.fit)$adj.r.squared),
      "\n", sep="")
  
  obj <- list()
  obj$results <- data.frame(Effect = c("Intercept", names), 
                            Coefficients = formatC(M.coeff, format="f", digits = 2),
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
  obj$r.squared <- formatC(summary(M.fit)$r.squared)
  obj$adj.r.squared <- formatC(summary(M.fit)$adj.r.squared)
  return(obj)
}