
#' @description Fit model by minimizing RSS + pen * length of fitted curve.
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


## foo <- function (y, parm, build, method = "L-BFGS-B", ..., debug = FALSE){
##     cl <- match.call()
##     cl[[1]] <- as.name("dlmMLE")
##     out <- eval(cl)
##     out$mod <- build(out$par, ...)
##     out$y <- y
##     return(out)    
## }


#' @description A wrapper for dlmFilter
#'
#' Not many details for now
#' @title Filter a dlm
#' @param object Result from calling dlmXMLE or dlmXOpt
#' @param debug FALSE
#' @param simplify FALSE
#' @return Same as when calling dlmFilter
#' @author Søren Højsgaard
#' @export
dlmXFilter <- function (object, debug = FALSE, simplify = FALSE){
  dlm::dlmFilter(object$y, object$mod, debug=debug, simplify=simplify)
} 



pickr  <- function(m, select, drop=TRUE){
    ## Select rows from matrix / dataframe and entries from list / vector
    ##print(head(m)); print(select)
    if (inherits(m, c("matrix", "data.frame"))) m[select,, drop=drop]
    else if (inherits(m, "list")) m[select]
    else m[select]
}

##' @description Extractor for dlmFiltered objects
##' @name extract-filtered
##' 
##' @title Extractor for dlmFiltered objects
##' @param x 
##' @param select 
##' @return 
##' @author Søren Højsgaard

#' @rdname extract-filtered
#' @export 
mm <- function(x, select=TRUE){UseMethod("mm")}

#' @rdname extract-filtered
#' @export 
mm.dlmFiltered <- function(object, select=TRUE){
    if (is.character(select)){
        if (identical(select, "last")) select <- NROW(object$m)
    }
    pickr(object$m, select, drop=TRUE)
}

#' @rdname extract-filtered
#' @export 
aa <- function(x, select=TRUE){UseMethod("aa")}

#' @rdname extract-filtered
#' @export 
aa.dlmFiltered <- function(object, select=TRUE){
    if (is.character(select)){
        if (identical(select, "last")) select <- NROW(object$a)
    }
    pickr(object$a, select, drop=TRUE)
}


#' @rdname extract-filtered
#' @export
CC <- function(x, select=TRUE){UseMethod("CC")}

#' @rdname extract-filtered
#' @export 
CC.dlmFiltered <- function(object, select=TRUE){
    if (is.character(select)){
        if (identical(select, "last")) select <- NROW(object$U.C)
    }
    out <- 
        dlmSvd2var(pickr(object$U.C, select, drop=FALSE),
                   pickr(object$D.C, select, drop=FALSE))
    if (is.numeric(select) && length(select) == 1) out[[1]] else out
}

#' @rdname extract-filtered
#' @export 
RR <- function(x, select=TRUE){UseMethod("RR")}

#' @rdname extract-filtered
#' @export 
RR.dlmFiltered <- function(object, select=TRUE){
    if (is.character(select)){
        if (identical(select, "last")) select <- NROW(object$U.R)
    }
    out <- 
        dlmSvd2var(pickr(object$U.R, select, drop=FALSE),
                   pickr(object$D.R, select, drop=FALSE))
    if (is.numeric(select) && length(select) == 1) out[[1]] else out    
}


