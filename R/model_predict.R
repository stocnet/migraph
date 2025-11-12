#' Predict methods for network regression
#' @param object An object of class inheriting "netlm"
#' @param newdata A design matrix with the same columns/variables as the
#'   fitted model.
#' @name predict
NULL

#' @rdname predict
#' @examples
#' networkers <- ison_networkers %>% to_subgraph(Discipline == "Sociology")
#' model1 <- net_regression(weight ~ ego(Citations) + alter(Citations) + sim(Citations), 
#'                       networkers, times = 20)
#' predict(model1, matrix(c(1,10,5,2),1,4))
#' @export
predict.netlm <- function(object, newdata = NULL) {
  # Extract coefficients
  coefs <- stats::coef(object)
  
  # If no newdata provided, use the original design matrix
  if (is.null(newdata)) {
    if (!is.null(object$X)) {
      newdata <- object$X
    } else {
      stop("No newdata provided and original design matrix not found in object.")
    }
  }
  
  # Ensure newdata is a matrix
  newdata <- as.matrix(newdata)
  
  # Compute predictions
  preds <- newdata %*% coefs
  
  return(drop(preds))
}

#' @rdname predict
#' @examples
#' networkers <- ison_networkers %>% to_subgraph(Discipline == "Sociology") %>% 
#'   to_unweighted()
#' model1 <- net_regression(. ~ ego(Citations) + alter(Citations) + sim(Citations), 
#'                       networkers, times = 20)
#' predict(model1, matrix(c(1,10,5,2),1,4))
#' @export
predict.netlogit <- function(object, newdata = NULL, type = c("link", "response")) {
  type <- match.arg(type)
  
  # Extract coefficients
  coefs <- stats::coef(object)
  
  # If no newdata provided, use the original design matrix
  if (is.null(newdata)) {
    if (!is.null(object$X)) {
      newdata <- object$X
    } else {
      stop("No newdata provided and original design matrix not found in object.")
    }
  }
  
  # Ensure newdata is a matrix
  newdata <- as.matrix(newdata)
  
  # Compute linear predictor
  eta <- newdata %*% coefs
  
  # Return either linear predictor or probability
  if (type == "link") {
    return(drop(eta))
  } else {
    return(drop(1 / (1 + exp(-eta))))
  }
}