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
dlmxOpt <- function(y, parm, build, pen, method = "L-BFGS-B", ...){
  
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
# #' @param debug FALSE
#' @return The result of calling optim padded with y and the fitted
#'     model object.
#' @author Søren Højsgaard

## ####################################################################

#' @export
dlmxMLE <- function (y, parm, build, method = "L-BFGS-B", ...) 
{
  logLik <- function(parm, ...) {
      mod <- build(parm, ...)
      if (!is.null(X(mod)) && (nrX <- NROW(X(mod))) < (nry <- NROW(y)))
          stop("X matrix does not have correct dimension: nrow of X: ", nrX, " length of y: ", nry, "\n")
      
      return(dlm::dlmLL(y = y, mod = mod, debug = TRUE))
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
#' @param object Result from calling dlmxMLE or dlmxOpt
## #' @param debug FALSE
#' @param simplify FALSE
#' @return Same as when calling dlmFilter
#' @author Søren Højsgaard
 
## ####################################################################



## ####################################################################

#' @description A wrapper for dlmFilter but restricted to using R code
#'     in the filter.
#' @title Filter a dlm
#' @param y The data. ‘y’ can be a vector, a matrix, a univariate or
#'           multivariate time series.
#' @param object An object of class ‘dlm’, or a list with components ‘m0’,
#'          ‘C0’, ‘FF’, ‘V’, ‘GG’, ‘W’, and optionally ‘JFF’, ‘JV’,
#'          ‘JGG’, ‘JW’, and ‘X’, defining the model and the parameters
#'          of the prior distribution.
#' @param simplify FALSE
#' @return Same as when calling dlmFilter
#' @author Søren Højsgaard
 
## ####################################################################

#' @export
dlmxFilter <- function (y, object, simplify = FALSE){
  dlm::dlmFilter(y, object, debug=TRUE, simplify=simplify)
} 




## summary.dlmFiltered <- function(object, ...){


    
## }

#' @export
print.dlm <- function(x, ...){
    cat("dynamic linear model (dlm)\n")
    
    invisible(x)   
}


## #' @export
## time_varying <- function(object, ...){
##     tvcn <- c("JFF", "JV", "JGG", "JW")
##     tvc  <- object[tvcn]
##     mm   <- !sapply(tvc, is.null)
##     uu   <- c("F","V","G","W")[mm]
##     uu
## }
    


#' @export
summary.dlm <- function(object, ...){


    cat("dynamic linear model (dlm)\n")
    cat("V | F: \n")
    print(cbind(V(object), FF(object)))

    cat("W | G: \n")
    print(cbind(W(object), GG(object)))

    tvcn <- c("JFF", "JV", "JGG", "JW")
    tvc  <- object[tvcn]
    mm   <- !sapply(tvc, is.null)
    uu   <- c("F","V","G","W")[mm]
    
    if (length(uu)==0) cat("Constant model\n")
    else cat("Time varying components:", toString(uu), "\n")

    if (length(uu)>0){
        zz <- tvc[mm]
        mapply(function(n, x){
            cat(n, ":\n")
            print(x)
        }, names(zz), zz )
    }
    
    invisible(object)
}







## foo <- function (y, parm, build, method = "L-BFGS-B", ..., debug = FALSE){
##     cl <- match.call()
##     cl[[1]] <- as.name("dlmMLE")
##     out <- eval(cl)
##     out$mod <- build(out$par, ...)
##     out$y <- y
##     return(out)    
## }














