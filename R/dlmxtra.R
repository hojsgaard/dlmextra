## ####################################################################

#' @description Fit model by minimizing RSS + pen * length of fitted
#'     curve.
#'
#' Not many details for now
#' @title Fit model by minimizing RSS + pen * length of fitted curve.
#' @param y Data
#' @param parm Intial value for parameter
#' @param build Build function
#' @param pen Penalty for length of fitted curve
#' @param method Method used by optim
#' @param ... Parameters passed on to build and optim
#' @return The result of calling optim padded with y and the fitted
#'     model object.
#' @author Søren Højsgaard

#' @import dlm
#' @importFrom dlm "dlmMLE" "dlmFilter" "dlmLL"
#' @importFrom stats "optim"
#' 

## ####################################################################

#' @export
dlmXOpt <- function(y, parm, build, pen, method = "L-BFGS-B", ...){
  
  eval_crit <- function(y, object, pen){
    filt <- dlm::dlmFilter(y, object)
    f <- filt$f
    crit <- sqrt(sum((y - f)^2)) + pen * sum(sqrt(diff(f)^2 + rep(1, length(y) - 1)))
    crit
  }
  
  crit_fun <- function(parm, ...){
    mod <- build(parm, ...)
    eval_crit(y, mod, pen)
  }
  out <- optim(parm, crit_fun, method = method, ...)
  out$mod <- build(out$par, ...)
  out$y <- y
  out$pen <- pen
  return(out)
}

## ####################################################################

#' @description A wrapper for calling dlmMLE
#'
#' Not many details for now
#' @title Fit dlm with MLE
#' @param y Data
#' @param parm Intial value for parameter
#' @param build Build function
#' @param method Method used by optim
#' @param ... Parameters passed on to build and optim
#' @param debug FALSE
#' @return The result of calling optim padded with y and the fitted
#'     model object.
#' @author Søren Højsgaard

## ####################################################################

#' @export
dlmXMLE <- function (y, parm, build, method = "L-BFGS-B", ..., debug = FALSE) 
{
  logLik <- function(parm, ...) {
    mod <- build(parm, ...)
    return(dlm::dlmLL(y = y, mod = mod, debug = debug))
  }
  out <- optim(parm, logLik, method = method, ...)
  out$mod <- build(out$par, ...)
  out$y <- y
  return(out)
}


## ####################################################################

#' @description A wrapper for dlmFilter
#'
#' Not many details for now
#' @title Filter a dlm
#' @param object Result from calling dlmXMLE or dlmXOpt
## #' @param debug FALSE
#' @param simplify FALSE
#' @return Same as when calling dlmFilter
#' @author Søren Højsgaard
 
## ####################################################################



## ####################################################################

#' @description A wrapper for dlmFilter but restricted to using R code
#'     in the filter.
#'
#' Not many details for now
#' @title Filter a dlm
#' @param y data
#' @param object a dlm object
## #' @param debug FALSE
#' @param simplify FALSE
#' @return Same as when calling dlmFilter
#' @author Søren Højsgaard
 
## ####################################################################

#' @export
dlmFilter2 <- function (y, object, simplify = FALSE){
  dlm::dlmFilter(y, object, debug=TRUE, simplify=simplify)
} 





## ####################################################################

#' @description A wrapper for dlmFilter
#'
#' Not many details for now
#' @title Filter a dlm
#' @param object Result from calling dlmXMLE or dlmXOpt
## #' @param debug FALSE
#' @param simplify FALSE
#' @return Same as when calling dlmFilter
#' @author Søren Højsgaard
 
## ####################################################################

#' @export
dlmXFilter <- function (object, simplify = FALSE){
  dlm::dlmFilter(object$y, object$mod, debug=TRUE, simplify=simplify)
} 





## foo <- function (y, parm, build, method = "L-BFGS-B", ..., debug = FALSE){
##     cl <- match.call()
##     cl[[1]] <- as.name("dlmMLE")
##     out <- eval(cl)
##     out$mod <- build(out$par, ...)
##     out$y <- y
##     return(out)    
## }














