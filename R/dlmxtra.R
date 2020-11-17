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


## foo <- function (y, parm, build, method = "L-BFGS-B", ..., debug = FALSE){
##     cl <- match.call()
##     cl[[1]] <- as.name("dlmMLE")
##     out <- eval(cl)
##     out$mod <- build(out$par, ...)
##     out$y <- y
##     return(out)    
## }

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



















pickr  <- function(m, select, drop=TRUE){
    ## Select rows from matrix / dataframe and entries from list / vector
    ##print(head(m)); print(select)
    if (inherits(m, c("matrix", "data.frame"))) m[select,, drop=drop]
    else if (inherits(m, "list")) m[select]
    else m[select]
}

## ####################################################################

##' @description Extractor for dlmFiltered objects
##' @name extract-dlmFiltered
##' 
##' @title Extractor for dlmFiltered objects
##' @param object A dlmFiltered object
##' @param select Which element to select
##' @return The relevant result
##' @author Søren Højsgaard

## ####################################################################

#' @rdname extract-dlmFiltered
#' @export 
mm <- function(object, select=TRUE){UseMethod("mm")}

#' @rdname extract-dlmFiltered
#' @export 
mm.dlmFiltered <- function(object, select=TRUE){
    if (is.character(select)){
        if (identical(select, "last")) select <- NROW(object$m)
    }
    pickr(object$m, select, drop=TRUE)
}

#' @rdname extract-dlmFiltered
#' @export 
aa <- function(object, select=TRUE){UseMethod("aa")}

#' @rdname extract-dlmFiltered
#' @export 
aa.dlmFiltered <- function(object, select=TRUE){
    if (is.character(select)){
        if (identical(select, "last")) select <- NROW(object$a)
    }
    pickr(object$a, select, drop=TRUE)
}


#' @rdname extract-dlmFiltered
#' @export
CC <- function(object, select=TRUE){UseMethod("CC")}

#' @rdname extract-dlmFiltered
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

#' @rdname extract-dlmFiltered
#' @export 
RR <- function(object, select=TRUE){UseMethod("RR")}

#' @rdname extract-dlmFiltered
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



## ####################################################################

##' @description Extractor for dlm objects
##' @name extract-dlm
##' 
##' @title Extractor for dlm objects
##' @param mod dlm object
##' @param i Row in X matrix 
##' @return The relevant result
##' @author Søren Højsgaard
##'
## ####################################################################

#' @rdname extract-dlm
#' @export 
getFF <- function(mod, i){
    if (is.null(JFF(mod))) return(FF(mod))

    FFF <- FF(mod)    
    JFFF <- JFF(mod)
    FFF[JFFF !=0] <- X(mod)[i,]
    FFF
}

#' @rdname extract-dlm
#' @export 
getGG <- function(mod, i){
    if (is.null(JGG(mod))) return(GG(mod))
    
    GGG <- FF(mod)
    JGGG <- JFF(mod)
    GGG[JGGG !=0] <- X(mod)[i,]
    GGG
}

#' @rdname extract-dlm
#' @export 
getVV <- function(mod, i){
    if (is.null(JV(mod))) return(V(mod))
}

#' @rdname extract-dlm
#' @export 
getWW <- function(mod, i){
    if (is.null(JW(mod))) return(W(mod))
}
