
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
##' @param object A dlmFiltered object.
##' @param slot Which slot in the object to select.
##' @param select Which element to select.
##' @return The relevant result
##' @author Søren Højsgaard

## ####################################################################

#' @rdname extract-dlmFiltered
#' @export 
getdlmf <- function(object, slot, select=TRUE){
    UseMethod("getdlmf")
}

#' @rdname extract-dlmFiltered
#' @export 
getdlmf.dlmFiltered <- function(object, slot, select=TRUE){
    switch(slot,
           "mm"=,"m"={mm(object, select=select)},
           "aa"=,"a"={aa(object, select=select)},
           "CC"=,"C"={CC(object, select=select)},
           "RR"=,"R"={RR(object, select=select)}
           )
}

## #' @rdname extract-dlmFiltered
## #' @export 
## mm <- function(object, select=TRUE){UseMethod("mm")}

## #' @rdname extract-dlmFiltered
## #' @export 
## mm.dlmFiltered
mm <- function(object, select=TRUE){
    if (is.character(select)){
        if (identical(select, "last")) select <- NROW(object$m)
    }
    pickr(object$m, select, drop=TRUE)
}

## #' @rdname extract-dlmFiltered
## #' @export 
## aa <- function(object, select=TRUE){UseMethod("aa")}

## ' @rdname extract-dlmFiltered
## ' @export 
## aa.dlmFiltered
aa <- function(object, select=TRUE){
    if (is.character(select)){
        if (identical(select, "last")) select <- NROW(object$a)
    }
    pickr(object$a, select, drop=TRUE)
}

## #' @rdname extract-dlmFiltered
## #' @export
## CC <- function(object, select=TRUE){UseMethod("CC")}

## #' @rdname extract-dlmFiltered
## #' @export 
## CC.dlmFiltered
CC <- function(object, select=TRUE){
    if (is.character(select)){
        if (identical(select, "last")) select <- NROW(object$U.C)
    }
    out <- 
        dlmSvd2var(pickr(object$U.C, select, drop=FALSE),
                   pickr(object$D.C, select, drop=FALSE))
    if (is.numeric(select) && length(select) == 1) out[[1]] else out
}

## #' @rdname extract-dlmFiltered
## #' @export 
## RR <- function(object, select=TRUE){UseMethod("RR")}

## #' @rdname extract-dlmFiltered
## #' @export 
## RR.dlmFiltered
RR <- function(object, select=TRUE){
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
##' @title  Extractor for dlm objects
##' @param  object dlm object
##' @param slot Which slot in the object to select.
##' @param  i Row in X matrix 
##' @return The relevant result
##' @author Søren Højsgaard
##'
## ####################################################################

#' @rdname extract-dlm
#' @export 
getdlm <- function(object, slot, i){
    UseMethod("getdlm")
}

#' @rdname extract-dlm
#' @export 
getdlm.dlm <- function(object, slot, i){
    if (!is.numeric(i) || length(i) > 1) stop("invalid index i\n")
    switch(slot,
           "FF"=,"F"={getFF(object, i)},
           "GG"=,"G"={getGG(object, i)},
           "VV"=,"V"={getGG(object, i)},
           "WW"=,"W"={getWW(object, i)}
           ) 
}

#' @rdname extract-dlm
#' @export 
getFF <- function(object, i){
    if (is.null(JFF(object))) return(FF(object))
    FFF <- FF(object)    
    JFFF <- JFF(object)
    FFF[JFFF !=0] <- X(object)[i,]
    FFF
}

#' @rdname extract-dlm
#' @export 
getGG <- function(object, i){
    if (is.null(JGG(object))) return(GG(object))
    GGG <- GG(object)
    JGGG <- JGG(object)
    GGG[JGGG !=0] <- X(object)[i,]
    GGG
}

#' @rdname extract-dlm
#' @export 
getVV <- function(object, i){
    if (is.null(JV(object))) return(V(object))
}

#' @rdname extract-dlm
#' @export 
getWW <- function(object, i){
    if (is.null(JW(object))) return(W(object))
}
